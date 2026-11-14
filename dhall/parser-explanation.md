# How the Haskell parser follows the Dhall grammar without copying it

This note explains the current Megaparsec parser in `dhall/src/Dhall/Parser/` and how it relates to the official grammar in `dhall/dhall-lang/standard/dhall.abnf`.

The grammar file is the definition of the language: which texts are Dhall programs, and how those texts are built out of smaller pieces. The Haskell code accepts the same texts and builds the same programs. It does not do the work in the same order, and it does not take the text one character at a time when a whole stretch of similar characters will do.

That difference is intentional. A parser that followed the grammar file one rule at a time would be correct and slow. Real Dhall files are full of spaces, names, operators, and long strings. The slow version repeats the same failed attempts at every one of those places. The Haskell version looks at the next character first, then runs only the rules that character can possibly start. For long stretches of ordinary characters it reads the stretch in one step.

The rest of this note walks through each of those changes. For each one it says what the grammar file asks for, what the Haskell does instead, why a successful parse is still the same program, and why the change is faster.

## Two descriptions of the same language

The grammar file is written in ABNF, a compact way of writing "this piece of text is one of the following shapes." A line like

```abnf
whitespace-chunk =
      " "
    / tab
    / end-of-line
    / line-comment
    / block-comment
```

means: a whitespace chunk is a space, or a tab, or the end of a line, or a line comment, or a block comment. The slash means "or", and the alternatives are tried from top to bottom. A star in front of a name, as in `*whitespace-chunk`, means "this piece, repeated zero or more times."

The Haskell parser is a set of functions that read those same shapes. The top-level function reads any leading `#!` lines, then whitespace, then one expression, then the end of the input. That matches the grammar's `complete-dhall-file` rule.

Where the two descriptions part ways is in the *attempts*. The grammar file is happy to say "try a date; if that fails, try a decimal number; if that fails, try a name." A direct translation tries the date parser on every name. The date parser fails on the first letter, the parser moves back to where it started, and the next alternative begins. Each failure is cheap on its own. A large file does it millions of times, and the failures become most of the work.

The Haskell keeps the same order of *successful* alternatives. It inserts a cheap question in front of the expensive ones: "given this next character, which alternatives are even possible?" Alternatives that cannot start with that character are not started.

A second kind of change replaces "one character, repeated" with "the longest stretch of characters of this kind." The grammar file writes a string as a list of single characters so that the language definition stays simple. The Haskell reads a run of ordinary characters as one slice of text. Joining those characters back together gives the same string the grammar describes.

Nothing in these changes adds a program the grammar rejects, or rejects a program the grammar accepts. What can differ is the wording of a syntax error, because the parser may stop at a different failed attempt and because a few common mistakes have their own messages. A text that is valid Dhall still parses to the same expression.

## Why a literal reading is slow

Three habits of a literal reading show up over and over in the sections below.

**Trying every alternative.** At each point in the file the grammar often lists many shapes. Most of them fail immediately on the next character. The cost is not the failure itself. The cost is entering the alternative, setting up the attempt, noticing the failure, and restoring the input position so the next alternative can start at the same character.

**Reading one character as its own step.** A rule written as "a digit, repeated" or "a comment character, repeated" describes a list of single characters. Doing that as a loop of "read one character, build a new one-character string, append it to the result" allocates a new little string per character. For a comment or a string of a few characters this is invisible. For a multi-megabyte string or a block comment it dominates the parse.

**Updating the line and column one character at a time.** The parser remembers where it is, as a line number and a column number, so error messages and source spans can point at the right place. The straightforward way to keep that up to date is to look at every character: a newline bumps the line and resets the column, anything else bumps the column. That walk is pure overhead when the caller only needed to skip a long stretch that cannot contain a newline.

The changes below attack those three habits. They do not change which strings are whitespace, which digit strings are numbers, or which names are built-in functions.

## Whitespace: one run, chosen by the first character

The grammar says a stretch of whitespace is a list of chunks, and each chunk is exactly one of: a single space, a tab, a newline, a `--` comment, or a `{-` comment.

The Haskell function `whitespaceChunk` looks at the next character and picks one branch:

- a space, a tab, or a newline starts a single run of spaces, tabs, and newlines
- a carriage return must be the start of a `\r\n` newline
- a dash starts a line comment
- an opening brace starts a block comment
- any other character is not whitespace

A run of spaces and tabs is the same text as many one-character chunks in a row. The grammar's list of chunks and the Haskell's single run describe the same characters. Comments stay separate chunks, because a comment is not a space: the run stops when the next character is `-` or `{` or anything else that is not space, tab, or newline.

The speed comes from the first-character choice. A literal reading of the five-way "or" tries "is this a space?" and, on failure, "is this a tab?", then "is this a newline?", then "is this a comment?". In a real file the next character is usually a letter, a comma, a parenthesis, or the end of a line. All five attempts fail. Looking at the character once and returning immediately skips those failed attempts. Spaces between tokens are still consumed as one run, which is what Megaparsec's "take while this is a space" already did; the new part is not attempting the comment parsers when the character cannot start a comment.

The same idea applies to the required space after `:`. The grammar says there must be at least one whitespace chunk there, so that `x : Natural` is an annotation and a missing space is not silently accepted. The Haskell uses that same requirement, and when the space is missing it reports "Whitespace is required after :" instead of a generic failure. The language is unchanged. The message is aimed at a mistake that shows up constantly in hand-written Dhall.

## Long stretches and the line/column counter

Several of the fast paths below read a long slice of the input in one step. After reading it, the parser still has to know the line and column of the next character.

Megaparsec's usual "take while" does that by walking the slice. For a short gap that walk is tiny. For a ten-million-character string it is the whole cost of the parse.

`spanWhile` and `skipChunk` in `Dhall/Parser/Combinators.hs` replace that walk with three cases:

- If the slice is empty, nothing happens.
- If the slice is 16 characters or fewer, the parser only records "the input moved forward by this many characters." It does not compute a new line and column yet. The next time something asks for the position, Megaparsec catches up from the last known position. A single space between tokens takes this path, so ordinary files do not pay for a position update on every space.
- If the slice is longer than 16 characters and it cannot contain a newline or a tab, the column is increased by the length of the slice. That is one addition, not a loop over the characters. This applies to ordinary string contents and to digit runs, because those predicates reject newlines and tabs, so the parser knows the slice is one line without looking at it.
- If the slice is long and it might contain a newline or a tab, the parser walks the slice once, updating the line and column as it goes. Block comments take this path when they actually contain a newline, because a comment is allowed to span lines. The win for comments is described in the comment section: the walk that finds the end of the comment is no longer a walk that also builds the comment text one character at a time.

The threshold of 16 is a tradeoff. Below it, the catch-up walk that Megaparsec does later is as cheap as doing the work now, and skipping the position update avoids a function call on the millions of one-space gaps in a large file. Above it, a newline-free slice is long enough that adding the length in one step is cheaper than visiting every character.

## Strings: a slice of ordinary characters

The grammar describes a double-quoted string as a list of chunks. Each chunk is one of: an interpolation `${ ... }`, an escape such as `\n`, or one ordinary character. An ordinary character is anything except the closing quote and the backslash. A single-quoted string is the same idea, with different rules for quotes and escapes.

A literal parser would take the string `"hello"` as five separate ordinary-character chunks and then concatenate them.

The Haskell `doubleQuotedChunk` parser tries, in the same order as the grammar:

1. an interpolation, if the input starts with `${`
2. otherwise the longest run of ordinary characters that are also not `$`
3. otherwise a single `$`
4. otherwise an escape starting with `\`

The longest run is step 2, implemented with `spanWhile1`. The predicate is the grammar's ordinary-character test, plus "not a dollar sign." Excluding `$` is what keeps interpolations working. The characters of `hello` are all ordinary and none of them is `$`, so they come back as one slice. A `$` stops the slice so that `${` can be recognized as an interpolation on the next attempt, and a lone `$` is then accepted by the single-character step. A `"` or `\` also stops the slice, so the closing quote and escapes stay where the grammar puts them.

Concatenating the slices and the single-character pieces produces the same string the grammar describes. The difference is allocation. One slice of a million characters is one string. A million one-character strings, each appended to the previous result, copies a growing buffer on every character.

Single-quoted strings use the same pattern. The fast run stops at `$` and at `'`, because those are the characters that can start an interpolation, an escaped quote, or the closing `''`. Newlines and tabs inside a single-quoted string are their own alternatives, matching the grammar, so the fast run does not swallow them.

## Block comments: find the end, then skip

The grammar says a block comment is `{-`, then a continuation. The continuation is the closing `-}` , or a nested `{- ... -}` followed by more continuation, or one ordinary comment character followed by more continuation. Nesting is real: `{- outer {- inner -} still outer -}` is one comment.

A literal reading of "one character, then continue" walks the body one character at a time and builds the comment text by appending each character. It also re-enters the continuation rule on every character, including the long stretches that contain neither `{` nor `-` and therefore cannot start a nested comment or a closer.

`takeBlockComment` walks the text with a nesting depth instead:

- It looks for the next `-` or `{` in one slice, skipping everything in between. Characters that are neither cannot close the comment or open a nested one.
- A `-` followed by `}` decreases the depth. At depth zero the comment is over, and the function returns how many characters that took, including the closer.
- A `{` followed by `-` increases the depth and the scan continues inside the nested comment.
- A `-` or `{` that is not part of a delimiter is an ordinary character and the scan resumes after it.

If the walk reaches the end without returning to depth zero, the comment is unterminated and the parse fails, as the grammar requires.

The caller then drops that many characters from the input in one step via `skipChunk`, and returns `"{-"` joined with the slice. Joining is lazy. The whitespace parser throws the returned text away, so parsing ordinary code does not copy the comment body. `dhall-docs` does look at the text, because a comment that starts with `{-|` is a documentation comment. The string is there when a caller forces it, and absent as real work when nobody asks for it.

The accepted comments are the same nested comments the grammar describes. The speed comes from skipping delimiter-free stretches in one slice, and from not building a one-character string per character of the body.

## Numbers: one digit run, then a conversion that splits in half

The grammar lists numeric literals in an order that needs backup. A date, a double such as `1.0` or `1e2`, a natural number, and an integer all share leading digits. The grammar says to try the date and the double first, and to back up if that attempt fails, because a plain `42` is a natural number and must not be rejected just because it is not a double.

A literal double parser, faced with a million digits and no decimal point, reads every digit, notices that there is no `.` and no exponent, fails, and rewinds. The natural-number parser then reads every digit again.

The Haskell splits this by the first character, and for a non-zero decimal it reads the digits once:

- A `"` starts a string. No numeric parser runs.
- A `{`, `<`, `[`, or `` ` `` starts a record, a union, a list, or a quoted name.
- A `+` or `-` starts a signed number: a temporal offset, a double, or an integer.
- A `0` starts a bytes literal (`0x"`), a temporal literal, a `0x` or `0b` natural, or the natural number zero. A `0` followed by more decimal digits is rejected, because the grammar forbids leading zeros.
- A digit from `1` to `9` starts an unsigned number. The parser tries a temporal literal first (so `2020-01-01` is still a date), then a decimal natural, then a double.
- The decimal natural reads the whole digit run with `spanWhile1` and then checks that what follows is not a fraction or an exponent. `1.0` and `1e2` fail that check and fall through to the double parser. `123` succeeds as a natural. `123.foo` also succeeds as a natural, because a field selection is not a fraction: the grammar's double requires a digit after the dot. The `.` is parsed later as field access.

So the successful cases stay in the grammar's order. A double is still a double. A date is still a date. A plain digit string is a natural, and its digits are taken from the input once.

Turning that digit string into a number is a separate cost, and it matters only for very long literals. Adding one digit at a time means each new digit copies the entire number built so far. For a number with millions of digits that copying is quadratic: the first digits are copied again on every later digit. `naturalFromDecimalDigits` avoids that. A run of 18 digits or fewer fits in a machine word and is converted with an ordinary loop. A longer run is split in half, the halves are converted, and the results are combined as `high * 10^k + low`. The same pattern, with shifts instead of powers of ten, converts hexadecimal and binary runs. Short numbers, which are almost all numbers in real files, take the simple loop.

One more choice sits next to this and is not a grammar change. When the parser wraps an expression with its source span, it does not force the expression's value. Asking only "did this parse?" leaves a ten-million-digit natural as the digit string's conversion, unevaluated, until something actually needs the number. Forcing the value still builds it. The benchmark that parses a huge natural without using it stays cheap for that reason; the benchmark that prints the number pays the conversion.

## Values: the first character picks the shape

The grammar's `primitive-expression` rule is a long ordered list: a temporal literal, a double, a natural, an integer, a text, a bytes literal, a record, a union, a list, an identifier, or a parenthesized expression. Several of the numeric alternatives are marked "back up if this fails", because they share prefixes.

`primitiveExpression` looks at one character and calls only the group that can start with it. A letter goes to names and built-ins (and to `Infinity`, which is a digit-free double). A quote goes to strings. An opening brace goes to records. A parenthesis is handled by the surrounding alternative, which is the grammar's `(` expression `)`. Anything that does not match one of those fast cases falls through to the full ordered list, so an unusual but legal start is still tried in the grammar's order.

Built-in names get the same treatment one level down. The grammar says an identifier is a variable or a built-in, and a built-in is one of a fixed list of names (`Natural`, `List/fold`, `True`, and so on). A literal reading tries that whole list at every name. The Haskell looks at the first letter and tries only the built-ins that start with it. A name starting with `x` tries none of them: `x` cannot be `Natural` or `List` or `True`. The name is parsed as a variable. A name starting with `N` tries `Natural/fold`, `Natural`, `None`, `NaN`, and the other `N` names, longest-first, which is the same order the grammar needs so that `Natural/fold` is not misread as the type `Natural` followed by junk.

The programs are the same because every alternative is still present, just not entered when the next character makes it impossible. A letter is not a digit, so it is not a date. A quote is not a brace, so it is not a record. Skipping those attempts does not change the first alternative that succeeds.

## Operators: skip the whole tower when the next character cannot be one

The grammar encodes operator precedence as a tower of rules. An equivalence (`===`) is a list of import-alternatives (`?`) separated by `===`. An import-alternative is a list of or-expressions separated by `?`. This continues down through `||`, `+`, `++`, `#`, `&&`, `//`, `⫽`, `//\\`, `*`, `==`, and `!=`, until the bottom is a function application. Each level says "the next level, then zero or more (whitespace, this operator, the next level)."

That tower is why `1 + 2 * 3` is `1 + (2 * 3)` and `True && False || True` groups the way the standard says. The Haskell builds the same tower: thirteen layers, each one a loop of "operator, then the next layer down", folded from the left so that `1 + 2 + 3` is `(1 + 2) + 3`.

A literal reading pays for the tower even when there is no operator. After every small expression the parser is usually looking at a comma, a closing brace, a closing parenthesis, a colon, or the end of the file. None of those is an operator. The literal tower still tries each of the thirteen layers, and each layer tries its operator, fails, and backs up.

`makeOperatorExpression` looks ahead through whitespace at the next character. If that character cannot start any operator, it returns the expression it already has and does not enter the loop. The characters that can start an operator are `=`, `≡`, `?`, `|`, `+`, `#`, `&`, `/`, `∧`, `⫽`, `⩓`, `*`, and `!`. A comma is not on that list. A letter is not on that list. The tower is skipped.

When the character *is* a possible operator, the layer still does not try every operator. Each layer checks the first character and runs only the operator parser that starts with it. A `+` tries `+` and, on a different layer, `++`. A `#` does not try `===`.

Two operators share a first character and are kept in grammar order. `==` and `===` both start with `=`. The `==` parser accepts two equals signs only when a third one does not follow, so `===` remains the equivalence operator and `==` remains equality. `+` with a required following space is addition; `++` is text append. The grammar uses a required space after `+` so that `f +2` is "apply `f` to the integer `+2`" and not addition. The Haskell keeps that required space.

The grouping of a chain is the grammar's grouping. The speed is the skipped attempts after every expression that is not followed by an operator, which in real files is most expressions.

## Function application: decide there is another argument before parsing it

The grammar says an application is a first expression, then zero or more further expressions separated by whitespace. The first expression may be `merge`, `Some`, `toMap`, or `showConstructor` applied to its arguments, or it may be an ordinary import-expression. Import-expressions are themselves imports or selections (`record.field`) or parenthesized expressions, and so on.

The awkward case is the space. In `f x y` the spaces separate arguments. In `f x in y` the space before `in` is the space the `let` grammar requires; `in` is not another argument of `f`. In `foo./bar` there is no space, and the grammar says application arguments must be separated by whitespace, so this is not "apply `foo.` to `bar`". A literal "keep parsing arguments until parsing an argument fails" will try to parse `in` as an expression, fail part way, and back up. Done at every application, that failed attempt is a large part of parsing a file full of names.

The Haskell loop peeks first. After whitespace, `peekImportExpressionStart` asks whether the next characters can start an import-expression. A quote, brace, bracket, parenthesis, angle bracket, or backtick can. A digit can. A `+` or `-` can when a digit follows. `.`, `..`, `/`, and `~` can when they start a path. A name can, unless the name is a keyword that continues an outer expression: `then`, `else`, `in`, `with`, `as`, `using`, `let`, `if`, `merge`, `Some`, `toMap`, `assert`, `forall`. If the peek says no, the argument loop stops, and the outer grammar (`let`, `if`, a record comma, and so on) sees that keyword or delimiter exactly as it would have if the argument parse had been attempted and failed.

If the peek says yes, the argument is parsed for real, and a syntax error inside it is reported there. The peek is the only part that is allowed to fail and be undone. That matches the grammar's "another argument, or stop", and it avoids parsing a keyword as a failed expression on every application in the file.

Field selection uses a smaller version of the same idea. The grammar says to back up when a dot is not actually a field, a projection, or a type selector, because `foo./bar` is an application of `foo` to a path once whitespace rules allow it. The Haskell only treats a dot as a selector when the next character can start a label, a `{` projection, or a parenthesized type. A dot that starts a path is left for the path parser.

## `->`, `:`, and `with` share one reading of the left-hand side

The grammar's top-level `expression` rule lists many forms that begin with an operator expression and then diverge:

- `a -> b`, a function type
- `a with x = b`, an update
- `merge e1 e2 : t` and `toMap e : t`, the same keywords with a type annotation
- `[] : t`, an empty list with its element type
- `x : t`, an annotation
- or just an operator expression by itself

Several of these are marked "back up if this fails", because the parser cannot know from the start which form it is in. A literal reading parses the left-hand expression once for the arrow rule, fails, backs up, parses the same text again for the `with` rule, fails, backs up, and parses it a third time as an annotation.

The Haskell parses that left-hand side once, as an application (which includes the operator tower). It remembers a small amount of extra information: was this a bare `merge`, `Some`, or `toMap` with no extra arguments, or a bare import-expression? Then it looks at what follows.

- Whitespace and `with` only continue if that left-hand side was an import-expression, which is what the grammar's `with` rule requires. The clauses after `with` are operator expressions, again matching the grammar.
- An arrow builds a function type. The grammar writes `a -> b` with a real left-hand side; the Haskell stores that as a function type whose bound variable is `_`, which is the same encoding this parser has used for that form.
- A colon and a type handle the special cases the grammar spells out as separate rules. An empty list becomes `[] : t`. A bare `merge` or `toMap` grows a type annotation. Anything else is an ordinary annotation `x : t`.
- If none of those follow, the expression is just the left-hand side. An empty list that never gets a type is an error, as the grammar requires, and the message says so directly.

One reading of `a`, then a decision based on the next token, accepts the same forms as several grammar alternatives that each re-read `a` and back up.

## What this does not change

The shortcuts are about attempts and about reading a stretch at once. They are not a different language.

- Leading `#!` lines are still allowed only as a prefix of the file, and a trailing `--` comment is still allowed after the expression.
- Keywords stay keywords. `if`, `let`, `forall`, `merge`, `assert` and the rest are recognized as the grammar recognizes them, including the required spaces that keep `env:FOO` from being read as an annotation and `http://...` from being read as a `//` operator.
- Operator precedence and associativity are the tower from the grammar.
- Imports, paths, hashes, record literals, record types, unions, lists, and interpolations are still the same shapes. A few of them dispatch on the first character the same way primitive expressions do, so a `"` is a string and not a failed number, but the rules for what a string may contain are the grammar's rules.
- Source spans are still recorded. Every expression node remembers the start position, the end position, and the text it came from. Operators and applications rebuild that text by joining the texts of their parts, so a span still covers the whole operator expression or the whole application.

The practical effect on a large, ordinary file is mostly the skipped attempts: whitespace that does not try to be a comment, names that do not try to be numbers, and expressions that do not try every operator. The practical effect on a huge string, a huge block comment, or a huge numeric literal is the bulk read: one slice, a line and column update that does not walk the slice when the slice cannot contain a newline, and a number conversion that splits long digit strings in half instead of appending one digit at a time.
