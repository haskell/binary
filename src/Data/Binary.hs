{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      : Data.Binary
-- Copyright   : Lennart Kolmodin
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Lennart Kolmodin <kolmodin@gmail.com>
-- Stability   : unstable
-- Portability : portable to Hugs and GHC. Requires the FFI and some flexible instances
--
-- Binary serialisation of Haskell values to and from lazy 'ByteString's.
-- The Binary library provides methods for encoding Haskell values as
-- streams of bytes directly in memory. The resulting 'ByteString' can
-- then be written to disk, sent over the network, or further processed
-- (for example, compressed with gzip).
--
-- The @Binary@ package is notable in that it provides both pure, and
-- high performance serialisation.
--
-- Values are always encoded in network order (big endian) form, and
-- encoded data should be portable across machine endianness, word size,
-- or compiler version. For example, data encoded using the 'Binary' class
-- could be written from GHC, and read back in Hugs.
--
-----------------------------------------------------------------------------

module Data.Binary (

    -- * The Binary class
      Binary(..)

#ifdef GENERICS
    -- * Generic support
    -- $generics
    , GBinary(..)
#endif
    -- $example

    -- * The Get and Put monads
    , Get
    , Put

    -- * Useful helpers for writing instances
    , putWord8
    , getWord8

    -- * Binary serialisation
    , encode                    -- :: Binary a => a -> ByteString
    , decode                    -- :: Binary a => ByteString -> a

    -- * IO functions for serialisation
    , encodeFile                -- :: Binary a => FilePath -> a -> IO ()
    , decodeFile                -- :: Binary a => FilePath -> IO a

-- Lazy put and get
--  , lazyPut
--  , lazyGet

    , module Data.Word -- useful

    ) where

import Data.Word

import Data.Binary.Class
import Data.Binary.Put
import Data.Binary.Get
#ifdef GENERICS
import Data.Binary.Generic ()
#endif

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L

------------------------------------------------------------------------

-- $example
-- To serialise a custom type, an instance of Binary for that type is
-- required. For example, suppose we have a data structure:
--
-- > data Exp = IntE Int
-- >          | OpE  String Exp Exp
-- >    deriving Show
--
-- We can encode values of this type into bytestrings using the
-- following instance, which proceeds by recursively breaking down the
-- structure to serialise:
--
-- > instance Binary Exp where
-- >       put (IntE i)          = do put (0 :: Word8)
-- >                                  put i
-- >       put (OpE s e1 e2)     = do put (1 :: Word8)
-- >                                  put s
-- >                                  put e1
-- >                                  put e2
-- >
-- >       get = do t <- get :: Get Word8
-- >                case t of
-- >                     0 -> do i <- get
-- >                             return (IntE i)
-- >                     1 -> do s  <- get
-- >                             e1 <- get
-- >                             e2 <- get
-- >                             return (OpE s e1 e2)
--
-- Note how we write an initial tag byte to indicate each variant of the
-- data type.
--
-- We can simplify the writing of 'get' instances using monadic
-- combinators:
--
-- >       get = do tag <- getWord8
-- >                case tag of
-- >                    0 -> liftM  IntE get
-- >                    1 -> liftM3 OpE  get get get
--
-- The generation of Binary instances has been automated by a script
-- using Scrap Your Boilerplate generics. Use the script here:
--  <http://darcs.haskell.org/binary/tools/derive/BinaryDerive.hs>.
--
-- To derive the instance for a type, load this script into GHCi, and
-- bring your type into scope. Your type can then have its Binary
-- instances derived as follows:
--
-- > $ ghci -fglasgow-exts BinaryDerive.hs
-- > *BinaryDerive> :l Example.hs
-- > *Main> deriveM (undefined :: Drinks)
-- >
-- > instance Binary Main.Drinks where
-- >      put (Beer a) = putWord8 0 >> put a
-- >      put Coffee = putWord8 1
-- >      put Tea = putWord8 2
-- >      put EnergyDrink = putWord8 3
-- >      put Water = putWord8 4
-- >      put Wine = putWord8 5
-- >      put Whisky = putWord8 6
-- >      get = do
-- >        tag_ <- getWord8
-- >        case tag_ of
-- >          0 -> get >>= \a -> return (Beer a)
-- >          1 -> return Coffee
-- >          2 -> return Tea
-- >          3 -> return EnergyDrink
-- >          4 -> return Water
-- >          5 -> return Wine
-- >          6 -> return Whisky
-- >
--
-- To serialise this to a bytestring, we use 'encode', which packs the
-- data structure into a binary format, in a lazy bytestring
--
-- > > let e = OpE "*" (IntE 7) (OpE "/" (IntE 4) (IntE 2))
-- > > let v = encode e
--
-- Where 'v' is a binary encoded data structure. To reconstruct the
-- original data, we use 'decode'
--
-- > > decode v :: Exp
-- > OpE "*" (IntE 7) (OpE "/" (IntE 4) (IntE 2))
--
-- The lazy ByteString that results from 'encode' can be written to
-- disk, and read from disk using Data.ByteString.Lazy IO functions,
-- such as hPutStr or writeFile:
--
-- > > writeFile "/tmp/exp.txt" (encode e)
--
-- And read back with:
--
-- > > readFile "/tmp/exp.txt" >>= return . decode :: IO Exp
-- > OpE "*" (IntE 7) (OpE "/" (IntE 4) (IntE 2))
--
-- We can also directly serialise a value to and from a Handle, or a file:
--
-- > > v <- decodeFile  "/tmp/exp.txt" :: IO Exp
-- > OpE "*" (IntE 7) (OpE "/" (IntE 4) (IntE 2))
--
-- And write a value to disk
--
-- > > encodeFile "/tmp/a.txt" v
--

------------------------------------------------------------------------
-- Wrappers to run the underlying monad

-- | Encode a value using binary serialisation to a lazy ByteString.
--
encode :: Binary a => a -> ByteString
encode = runPut . put
{-# INLINE encode #-}

-- | Decode a value from a lazy ByteString, reconstructing the original structure.
--
decode :: Binary a => ByteString -> a
decode = runGet get

------------------------------------------------------------------------
-- Convenience IO operations

-- | Lazily serialise a value to a file
--
-- This is just a convenience function, it's defined simply as:
--
-- > encodeFile f = B.writeFile f . encode
--
-- So for example if you wanted to compress as well, you could use:
--
-- > B.writeFile f . compress . encode
--
encodeFile :: Binary a => FilePath -> a -> IO ()
encodeFile f v = L.writeFile f (encode v)

-- | Lazily reconstruct a value previously written to a file.
--
-- This is just a convenience function, it's defined simply as:
--
-- > decodeFile f = return . decode =<< B.readFile f
--
-- So for example if you wanted to decompress as well, you could use:
--
-- > return . decode . decompress =<< B.readFile f
--
-- After contructing the data from the input file, 'decodeFile' checks
-- if the file is empty, and in doing so will force the associated file
-- handle closed, if it is indeed empty. If the file is not empty,
-- it is up to the decoding instance to consume the rest of the data,
-- or otherwise finalise the resource.
--
decodeFile :: Binary a => FilePath -> IO a
decodeFile f = do
    s <- L.readFile f
    return $ runGet (do v <- get
                        m <- isEmpty
                        m `seq` return v) s

-- needs bytestring 0.9.1.x to work

------------------------------------------------------------------------
-- Lazy put and get

-- lazyPut :: (Binary a) => a -> Put
-- lazyPut a = put (encode a)

-- lazyGet :: (Binary a) => Get a
-- lazyGet = fmap decode get

-- $generics
--
-- Beginning with GHC 7.2, it is possible to use binary serialization
-- without writing any instance boilerplate code.
--
-- > {-# LANGUAGE DeriveGeneric #-}
-- >
-- > import Data.Binary
-- > import GHC.Generics (Generic)
-- >
-- > data Foo = Foo
-- >          deriving (Generic)
-- >
-- > -- GHC will automatically fill out the instance
-- > instance Binary Foo
--
-- This mechanism makes use of GHC's efficient built-in generics
-- support.
