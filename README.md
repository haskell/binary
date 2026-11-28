# binary package

[![Hackage version](https://img.shields.io/hackage/v/binary.svg?label=Hackage)](https://hackage.haskell.org/package/binary) [![Stackage version](https://www.stackage.org/package/binary/badge/lts?label=Stackage)](https://www.stackage.org/package/binary) [![Haskell CI](https://github.com/haskell/binary/actions/workflows/haskell-ci.yaml/badge.svg)](https://github.com/haskell/binary/actions/workflows/haskell-ci.yaml)

*Pure binary serialisation using lazy ByteStrings.*

The ``binary`` package provides Data.Binary, containing the Binary class,
and associated methods, for serialising values to and from lazy
ByteStrings.
A key feature of ``binary`` is that the interface is both pure, and
moderately efficient.
The ``binary`` package is portable to GHC and MicroHs.

## Installing binary from Hackage ##

``binary`` is part of The Glasgow Haskell Compiler (GHC) and therefore if you
have GHC installed, you already have ``binary``.

More recent versions of ``binary`` than you might have installed may be
available from [Hackage](https://hackage.haskell.org/package/binary).

## Building and testing binary ##

``binary`` comes with both a test suite and a set of benchmarks in the
``binary-tests`` package (configured in ``cabal.project`` so test and
benchmark dependencies do not collide with GHC's boot ``binary`` package).

    $ git clone https://github.com/haskell/binary.git
    $ cd binary
    $ cabal update
    $ cabal build all

Run the test suite:

    $ cabal test all

Run a benchmark (for example ``put`` or ``get``):

    $ cabal bench binary-tests:bench:put

To build the Haddock documentation for ``binary.cabal`` directly:

    $ cabal haddock -z

## Using binary ##

First:

    import Data.Binary

and then write an instance of Binary for the type you wish to serialise.
An example doing exactly this can be found in the Data.Binary module.
You can also use ``Data.ByteString.Builder`` to efficiently build
lazy bytestrings using the ``Builder`` monoid. Or, alternatively,
``Data.Binary.Get`` and ``Data.Binary.Put`` to serialize/deserialize using
the ``Get`` and ``Put`` monads.

More information in the haddock documentation.

## Deriving binary instances, ``Generically`` ##

Beginning with GHC 9.4 it is possible to derive binary serialization
using the ``Generically`` newtype.

This is achieved by deriving an instance of ``Generic`` and then
deriving the appropriate ``Binary T`` instance via ``Generically T``.

```haskell
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}

import Data.Binary
import GHC.Generics (Generic, Generically(..))

data Foo = Foo
  deriving stock Generic
  deriving Binary via Generically Foo
```

Beginning with GHC 7.2, a generic definition has been a part of the
``Binary`` typeclass. This could also be derived using the
``anyclass`` strategy:

```haskell
data Foo = Foo
  deriving stock    Generic
  deriving anyclass Binary
```

Which means the same as an empty class declaration: ``instance
Binary Foo``.

## Contributors ##

* Lennart Kolmodin
* Duncan Coutts
* Don Stewart
* Spencer Janssen
* David Himmelstrup
* Björn Bringert
* Ross Paterson
* Einar Karttunen
* John Meacham
* Ulf Norell
* Tomasz Zielonka
* Stefan Karrmann
* Bryan O'Sullivan
* Bas van Dijk
* Florian Weimer

For a full list of contributors, see
[here](https://github.com/haskell/binary/graphs/contributors).
