# `dhall 1.9.1`

`dhall` is a total programming language specialized to configuration files

## Features:

* Haskell integration - Dhall expressions can be marshalled into Haskell
* Total - Programs always terminate and will never hang
* Safe - Programs never crash or throw exceptions
* Distributed - Expressions can reference other expressions by URL or path
* Strong normalization - Every expression can be reduced to a normal form
* Statically typed - Configuration files can be validated ahead-of-time
* Strongly typed - No coercions, casts or subtyping
* Built-in data types - Includes lists, anonymous records and anonymous unions

## Quick start

Given this Haskell program saved to `example.hs`:

```haskell
-- example.hs

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import Dhall

data Example = Example { foo :: Integer, bar :: Vector Double }
    deriving (Generic, Show)

instance Interpret Example

main :: IO ()
main = do
    x <- input auto "./config"
    print (x :: Example)
```

... which reads in this configuration file:

```bash
$ cat ./config
{ foo = 1
, bar = ./bar
}
```

... which in turn references this other file:

```
$ cat ./bar
[3.0, 4.0, 5.0]
```

... you can interpret the Haskell program like this:

```bash
$ stack install dhall
$ stack runghc example.hs
Example {foo = 1, bar = [3.0,4.0,5.0]}
```

You can also interpret Dhall programs directly using the installed command-line
compiler:

```bash
$ dhall
List/head Double ./bar
<Ctrl-D>
Optional Double

[3.0] : Optional Double
```

... and you can reference remote expressions or functions by their URL, too:

```bash
$ dhall
let null = https://ipfs.io/ipfs/QmcTbCdS21pCxXysTzEiucDuwwLWbLUWNSKwkJVfwpy2zK/Prelude/List/null
in  null Double ./bar
<Ctrl-D>
Bool

False
```

Now go read the
[Dhall tutorial](https://hackage.haskell.org/package/dhall/docs/Dhall-Tutorial.html)
to learn more

# Integrations

This section keeps track of what languages and file formats natively support the
Dhall configuration language:

Complete:

* [`dhall-haskell`](https://github.com/dhall-lang/dhall-haskell) -
  Haskell integration (this package)
* [`dhall-nix`](https://github.com/dhall-lang/dhall-nix) - Nix integration
* [`dhall-json`](https://github.com/dhall-lang/dhall-json) -
  JSON and YAML integration
* [`dhall-bash`](https://github.com/dhall-lang/dhall-bash) - Bash integration
* [`dhall-text`](https://github.com/dhall-lang/dhall-text) - Template engine

In progress:

* [`dhall-rs`](https://github.com/nanotech/dhall-rs) - Rust integration
* [`dhall-scala`](https://github.com/missingfaktor/dhall-scala) - Scala
  integration

## Design philosophy

Programming languages are all about design tradeoffs and the Dhall language uses
the following guiding principles (in order of descending priority) that help
navigate those tradeoffs:

* Polish

    The language should delight users.  Error messages should be fantastic,
    execution should be snappy, documentation should be excellent, and
    everything should "just work".

* Simplicity

    When in doubt, cut it out.  Every configuration language needs bindings to
    multiple programming languages, and the more complex the configuration
    language the more difficult to create new bindings.  Let the host language
    that you bind to compensate for any missing features from Dhall.

* Beginner-friendliness

    Dhall needs to be a language that anybody can learn in a day and debug
    with little to no assistance from others.  Otherwise people can't recommend
    Dhall to their team with confidence.

* Robustness

    A configuration language needs to be rock solid.  The last thing a person
    wants to debug is their configuration file.  The language should never hang
    or crash.  Ever.

* Consistency

    There should only be one way to do something.  Users should be able to
    instantly discern whether or not something is possible within the Dhall
    language or not.

The `dhall` configuration language is also designed to negate many of the common
objections to programmable configuration files, such as:

> "Config files shouldn't be Turing complete"

Dhall is not Turing-complete.  Evaluation always terminates, no exceptions

> "Configuration languages become unreadable due to abstraction and indirection"

Every Dhall configuration file can be reduced to a normal form which eliminates
all abstraction and indirection

> "Users will go crazy with syntax and user-defined constructs"

Dhall is a very minimal programming language.  For example: you cannot even
compare strings for equality.  The language also forbids many other common
operations in order to force users to keep things simple

The biggest issue with using Dhall as a configuration language is that there are
currently only Haskell bindings.  If you would like to contribute bindings to
another language then go for it, otherwise I will do my best to contribute them
as time permits.

## Development status

[![Build Status](https://travis-ci.org/dhall-lang/dhall-haskell.png)](https://travis-ci.org/dhall-lang/dhall-haskell)

The compiler is built upon a theoretically sound foundation, meaning that if
there are no bugs then the language will never crash and will always halt.
However, in practice the compiler needs to be battle-tested to weed out any
implementation bugs.

The initial release of the language includes every feature that I intended to
add to the language.  This means that the only new features that I will add from
this point onwards are those that users request.  Even then, I may still push
back on feature requests in order to simplify porting Dhall bindings to
languages other than Haskell.

I would also like to create a formal language standard for Dhall to also help
with porting bindings to other host languages.  If you would like to assist with
either standardizing the language or creating new bindings just let me know
through the issue tracker.

## Name

The language is named after a
[Dustman from the game Planescape: Torment](http://torment.wikia.com/wiki/Dhall)
who belongs to a faction obsessed with death (termination).

## License (BSD 3-clause)

    Copyright (c) 2016 Gabriel Gonzalez
    All rights reserved.
    
    Redistribution and use in source and binary forms, with or without modification,
    are permitted provided that the following conditions are met:
        * Redistributions of source code must retain the above copyright notice,
          this list of conditions and the following disclaimer.
        * Redistributions in binary form must reproduce the above copyright notice,
          this list of conditions and the following disclaimer in the documentation
          and/or other materials provided with the distribution.
        * Neither the name of Gabriel Gonzalez nor the names of other contributors
          may be used to endorse or promote products derived from this software
          without specific prior written permission.
    
    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
    ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
    (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
    ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
    SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
