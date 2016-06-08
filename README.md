Print
=====

[![Build Status](https://travis-ci.org/sdiehl/print.svg?branch=master)](https://travis-ci.org/sdiehl/print)

Simple printing with Text. This is a prototype, not production code.

```haskell
base             >= 4.6  && <4.10,
text             >= 1.2  && <1.3,
text-format      >= 0.3  && <0.4
```

### Functions

Replaces the Prelude function ``show``:

```haskell
show :: Show a => a -> String
```

... with a new one that uses Text.

```haskell
show :: Show a => a -> Text
```

### Instances

A Text-based Show replacement can be derived with GHC's new
``-XDeriveAnyClass``.

```haskell
{-# LANGUAGE DeriveAnyClass #-}

data Animal 
  = Dog
  | Cat
  deriving (Generic, Print.Show)
```

### GHCi

GHCI's default printing mechanism can be overloaded by adding the following to
your ``.ghci`` file. Anything typed at the console will then use the modern
``print`` function instead of the Prelude.

```haskell
import Print
:set -interactive-print=Print.print
```

### Building

**Stack (Recommended)**

```bash
$ stack build
$ stack repl print:exe:Example
```

**Cabal**

```bash
$ cabal sandbox init
$ cabal install --only-dependencies
$ cabal build
$ cabal repl exe:Example
```
