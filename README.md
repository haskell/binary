
binary: efficient, pure binary serialisation using lazy ByteStrings
-------------------------------------------------------------------

The ``binary`` package provides Data.Binary, containing the Binary class,
and associated methods, for serialising values to and from lazy
ByteStrings. 

A key feature of 'binary' is that the interface is both pure, and efficient.

The 'binary' package is portable to GHC and Hugs.

Building:

    runhaskell Setup.lhs configure
    runhaskell Setup.lhs build
    runhaskell Setup.lhs install

First:
    import Data.Binary

and then write an instance of Binary for the type you wish to serialise.
More information in the haddock documentation.

Deriving:

It is possible to mechanically derive new instances of Binary for your
types, if they support the Data and Typeable classes. A script is
provided in tools/derive. Here's an example of its use.

    $ cd binary 
    $ cd tools/derive 

    $ ghci -fglasgow-exts BinaryDerive.hs

    *BinaryDerive> :l Example.hs 

    *Main> deriveM (undefined :: Exp)

    instance Binary Main.Exp where
      put (ExpOr a b) = putWord8 0 >> put a >> put b
      put (ExpAnd a b) = putWord8 1 >> put a >> put b
      put (ExpEq a b) = putWord8 2 >> put a >> put b
      put (ExpNEq a b) = putWord8 3 >> put a >> put b
      put (ExpAdd a b) = putWord8 4 >> put a >> put b
      put (ExpSub a b) = putWord8 5 >> put a >> put b
      put (ExpVar a) = putWord8 6 >> put a
      put (ExpInt a) = putWord8 7 >> put a
      get = do
        tag_ <- getWord8
        case tag_ of
          0 -> get >>= \a -> get >>= \b -> return (ExpOr a b)
          1 -> get >>= \a -> get >>= \b -> return (ExpAnd a b)
          2 -> get >>= \a -> get >>= \b -> return (ExpEq a b)
          3 -> get >>= \a -> get >>= \b -> return (ExpNEq a b)
          4 -> get >>= \a -> get >>= \b -> return (ExpAdd a b)
          5 -> get >>= \a -> get >>= \b -> return (ExpSub a b)
          6 -> get >>= \a -> return (ExpVar a)
          7 -> get >>= \a -> return (ExpInt a)
          _ -> fail "no decoding"

Contributors:

    Lennart Kolmodin
    Duncan Coutts
    Don Stewart
    Spencer Janssen
    David Himmelstrup
    Björn Bringert
    Ross Paterson
    Einar Karttunen
    John Meacham
    Ulf Norell
    Tomasz Zielonka
    Stefan Karrmann
    Bryan O'Sullivan
    Bas van Dijk
    Florian Weimer
