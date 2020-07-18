# `dhall-docs`

:construction: **This tool is on development phase yet**

For installation or development instructions, see:

* [`dhall-haskell` - `README`](https://github.com/dhall-lang/dhall-haskell/blob/master/README.md)

## Introduction

This `dhall-docs` package provides a command-line utility that takes a dhall package or
file and outputs an HTML documentation of it.

## Features

`dhall-docs` can analyze your Dhall package (essentially a folder with several
`.dhall` files) to generate documentation. Specifically:

* Extracts documentation from each file's header comments (see [Comment format](#comment-format)).
* The generated documentation includes breadcrumbs to aid navigation.
* Create an index for each folder in your package listing the `.dhall` files
  in that folder alongside the "exported packages" (the contained folders).
* Extracts examples from assertions.
* Extracts the type of each Dhall file from the source code and renders it
  in the indexes.
* Renders the source code in each Dhall file's documentation.

To see a demo, visit the documentation for the [`Dhall Prelude`](https://hydra.dhall-lang.org/job/dhall-haskell/master/generate-dhall-docs/latest/download/1/docs).

## Usage

The easiest usage is the following:

```bash
dhall-docs --input ${PACKAGE-FOLDER}
```

`dhall-docs` will store the documentation in
`${XDG_DATA_HOME}/dhall-docs/${OUTPUT-HASH}-${PACKAGE-NAME}/`, where:

* `$XDG_DATA_HOME` environment variable comes from the
    [xdg](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
    specification. If it is not defined, `dhall-docs` will default to
    `~/.local/share/`.
* `OUTPUT-HASH` is the hash of the generated documentation. This is to make the
    folder [content-addressable](https://es.wikipedia.org/wiki/Content_Addressed_Storage).
    Also, it avoids overwriting the documentation folder when there was a change in
    the way it was generated.
* `PACKAGE-NAME` is the package name of your documentation. By default, it will
    be the basename of the `--input` folder, but you can override it via
    `--package-name`.

After generating the documentation, `dhall-docs` will create a symlink to the
documentation index at `./docs/index.html`. You can customize the location of
that symlink using `--output-link` flag, like this:

```bash
dhall-docs --input . --output-link ${OTHER_LINK}
```

For more information about the tool, check the `--help` flag.


## Documenting your packages

Documenting your package is essentially writing comments on your source code,
although there are some format rules that these need to obey to work
properly with `dhall-docs`. These rules aid `dhall-docs` in extracting the text
on the documentation that will be passed to our Markdown preprocessor to finally
render your documentation in HTML.

You can see examples of writing documentation on the [tests](./tasty/data/package)
folder.

### Documentation markup language

The markup language of the documentation is [CommonMark], which is a strict
Markdown flavor. `dhall-docs` uses [`mmark`], a package that parses
[CommonMark] and transforms it to HTML. This package follows (almost)
that specification.

### Block comments

Normal block comments in Dhall starts with `{-` and ends with `-}`:

```dhall
{- foo
     bar
       baz
-}
```

A `dhall-docs` block comment needs to start with `"{-|"` and a
newline. The newline can be either `\n` or `\r\n`. This means that the actual
documentation _must_ starts on the following line.

[CommonMark] supports some features that are sensitive to indentation (e.g.
[indented-code-blocks]). Indentation on `dhall-docs` block comments is roughly
the same to [Dhall multi-line strings]: the indentation is determined by the
longest common whitespace prefix between the lines on the block comment.

In all the following examples:

```dhall
{-|
foo
  bar
    baz
-}
```

```dhall
  {-|
foo
  bar
    baz
-}
```

```dhall
{-|
    foo
      bar
        baz
    -}
```

```dhall
{-|
    foo
      bar
        baz-}
```

... `dhall-docs` will extract the contents of the comment, stripping all the
common leading whitespace from all lines, returning the following text that will
be passed to our [CommonMark] parser:

```
foo
  bar
    baz
```

Similar to Dhall multi-line string literals, the `-}` position also affects the
final indentation, meaning that in the following sample:

```dhall
{-|
    foo
    bar
-}
```

The following text will be extracted:

```
    foo
    bar
```

Only spaces and tabs are taken into account in calculating the longest common
whitespace prefix. Other forms of whitespaces are not considered.

Empty lines are ignored when determining the indentation level but are preserved
on the extracted text. On the following example:

```dhall
{-|
foo

bar
-}
```

`dhall-docs` will extract the following text:

```
foo

bar
```

Note that a line that only contains trailing whitespace will be taken into
account when calculating the common indentation. The line between `foo` and `bar`
on the following example has leading whitespace (two (2) whitespaces):

```dhall
  {-|
    foo
  
    bar -}
```

meaning that the extracted text will be:

```
  foo

  bar
```

If there is no newline between the opening brace of a `dhall-docs` block comment
(i.e. `{-|`), that comment will be considered invalid: `dhall-docs` will not
render it on the documentation _and_ a warning will be logged on the console:

```dhall
  {-| foo
      bar -}
```

... although it's a valid Dhall block comment, it's invalid for `dhall-docs`.

To end, if the `|` character isn't immediately after the opening brace,
`dhall-docs` will ignore the comment without logging on the console. This lets
you separate your internal block comments from the ones that you want
`dhall-docs` to output in the final markup.

This is an example of a file documented using block comments:

```dhall
{-|
# My awesome package

This is the header of my package

* a list item in **bold**
  - a sub item in _italic_
  - a [link](https://example.com)
    * a `code` quote


A new paragraph

    an indented code block
-}
let myAwesomeFunction {- this will be ignored on the documentation -}
               = \(x : Natural) -> x + 1

in
{
  {-|
  foo
      bar
          baz
  -}
  myAwesomeFunction
}
```

### Line comments

Normal Dhall single-line comments start with `--` and ends at a newline. For
example:

```dhall
-- single-line comment
0
```

A `dhall-docs` valid single-line comment starts with `--|` and a single whitespace
character. For example:

```dhall
--| foo
```

You can span your documentation in several single-line comments by writing `--` and two
(2) whitespaces:

```dhall
--| foo
--  bar
--      baz
```

The `--` prefix defines the base indentation for any line. That is, on the above
example `dhall-docs` will extract the following contents:

```
foo
bar
    baz
```

If you would like to force an empty line on the documentation output, place an
empty single line comment in that place. On this example:

```dhall
--| foo
--
--  bar
```

`dhall-docs` will extract:

```dhall
foo

bar
```

The empty `--` line doesn't need the two (2) whitespaces.

If between two single-line comments there is one (1) or more empty lines, the
latter comments will be ignored. On this example:


```dhall
--| foo
--  bar

--  baz
```

`dhall-docs` will only capture this text:

```
foo
bar
```

In a set of subsequent single-line comments, all lines before the one that has
the `|` character will be ignored, meaning that if any line on the set doesn't
have the `|` marker, no text will be extracted for the generated documentation.

On this example:

```dhall
--  foo
--  bar
--| baz
--  qux
```

the following text will be extracted:

```dhall
baz
qux
```

and this set of single-line comments:

```dhall
--  foo
--  bar
--  baz
```

will be ignored by the tool.

The set of `--` lines needs to be aligned with the same number of preceding
characters, meaning that a set of lines like this:

```dhall
--| foo
  --  bar
 --  baz
```

is invalid: `dhall-docs` will ignore this on the generated documentation and
a warning will be logged on the console.

To end, if there are language tokens in-between a set of lines, the lines after
those tokens will be ignored. On this example:

```dhall
--| foo
,
--  bar
```

the extracted text will be the following:

    foo

Here is an example of using this type of comments in a Dhall file:

```dhall
--| # My header
--
--  * 1
--  * 2

let a --| foo
      --  bar
        = True

in { a }

```

### Mixing the two type of comments

In a single Dhall file, you can use any type of comments to document your code,
but there is a restriction.

Two subsequent `dhall-docs` comments (whether single-line or
block comments) are forbidden: `dhall-docs` will reject them _and_ log a warning
on the console, but you can freely place any non-dhall comment after a `dhall-docs`
comment. All of the following examples are invalid:

```dhall
{-|
  foo -}
{-|
  bar -}
```

```dhall
{-|
  foo -}
--| bar
```

```dhall
{-|
  foo -}
{- -}
{-|
  qux -}
```

```dhall
--| foo
--  bar
{-|
  qux -}
```

```dhall
--| foo
--  bar
{- -}
--| qux
--  baz
```

Note that non `dhall-docs` comments are ignored in the above examples.


### Validating the extracted text

After extracting the text on a valid `dhall-docs` comment, it will be passed
to [`mmark`]. If there is a [CommonMark] parse error, an error will be logged
on the console _but_ the extracted text will be rendered in the documentation exactly as it was extracted.

### Supported annotated elements

A limitation of the `dhall` parser is that it doesn't preserve _all_ the whitespaces
of a Dhall source file, but preserves the enough to write useful documentation.
`dhall-docs` supports the following comments:

* The header of a file:

    ```dhall
    {-| this is the header
    -}
    let a = 1
    in  a
    ```

## Development

### `ghci`

If you want to open the `ghci` repl on this package using `stack`, you have to
provide an additional flag:

```bash
stack ghci dhall-docs --flag dhall-docs:ghci-data-files
```

... otherwise the CSS and JS files won't be properly embedded

### Generated docs on Hydra

At this moment, the only package generated by `dhall-docs` on hydra job sets is
the Dhall Prelude.

If you're in a PR, you can see it after a successful hydra build by visiting:

https://hydra.dhall-lang.org/job/dhall-haskell/${PR_NUMBER}/generate-dhall-docs/latest/download/1/docs

If you want to see the latest generated docs on the `master` branch, visit:

https://hydra.dhall-lang.org/job/dhall-haskell/master/generate-dhall-docs/latest/download/1/docs

[CommonMark]: https://commonmark.org/
[indented-code-blocks]: https://spec.commonmark.org/0.12/#indented-code-blocks
[`mmark`]: https://hackage.haskell.org/package/mmark
[Dhall multi-line strings]: https://github.com/dhall-lang/dhall-lang/blob/master/standard/multiline.md#indentation
