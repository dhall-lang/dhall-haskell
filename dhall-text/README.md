# `dhall-text`

For installation or development instructions, see:

* [`dhall-haskell` - `README`](https://github.com/dhall-lang/dhall-haskell/blob/master/README.md)

## Introduction

This `dhall-text` package provides a `dhall-to-text` executable which you can
use to template text using the Dhall configuration language.

## Example

Suppose you save the following files to your current directory:

```haskell
$ cat Person
-- Dhall is a typed programming language

-- This file is the type of an anonymous record
{ name : Text, upvotes : Natural }
```

```haskell
$ cat people
-- Dhall natively supports lists and anonymous records

[ { name = "Maria" , upvotes = 14 }
, { name = "Jordan", upvotes =  2 }
, { name = "Pranav", upvotes =  1 }
]

{- This file has type:
  
       ./people : List { name : Text, upvotes : Natural }
  
   ... or just:
  
       ./people : List ./Person
-}
```

```haskell
$ cat make-item
    -- You can define anonymous functions in Dhall using a backslash (i.e. `\`)
    \(person : ./Person)  -- You can import any type or expression by its path

    {- Dhall supports multiline strings that strip leading whitespace and Dhall
       supports string interpolation, too, using `${...}` syntax
    -}
->  ''
    <li class="list-group-item">
      <span class="badge">${Natural/show person.upvotes}</span>
      ${person.name}
    </li>
    ''

{- This file has type:
  
       ./make-item : ./Person -> Text
-}
```

```haskell
$ cat make-items
-- You can also import any type or expression by its URL
let List/map = https://prelude.dhall-lang.org/List/map

let Text/concat = https://prelude.dhall-lang.org/Text/concat

in  \(people : List ./Person) ->
        Text/concat (List/map ./Person Text ./make-item people)

{- This file has type:
  
       ./make-items : List ./Person -> Text
-}
```

Templating HTML is just ordinary function application:

```bash
$ dhall-to-text <<< './make-items ./people'
<li class="list-group-item">
  <span class="badge">14</span>
  Maria
</li>

<li class="list-group-item">
  <span class="badge">2</span>
  Jordan
</li>

<li class="list-group-item">
  <span class="badge">1</span>
  Pranav
</li>

```

To learn more about the Dhall configuration language,
[read the tutorial](https://hackage.haskell.org/package/dhall/docs/Dhall-Tutorial.html)
