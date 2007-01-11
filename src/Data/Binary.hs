-----------------------------------------------------------------------------
-- |
-- Module      : Data.Binary
-- Copyright   : Lennart Kolmodin
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Lennart Kolmodin <kolmodin@dtek.chalmers.se>
-- Stability   : unstable
-- Portability : FFI + (currently) flexible instances
--
-- Binary serialisation of values to and from lazy ByteStrings.
--
-----------------------------------------------------------------------------

module Data.Binary (
      Binary(..)                -- class Binary

    -- Instances Binary for: () Bool Word8-64 Int8-64 Int

    , encode                    -- :: Binary a => a -> ByteString
    , decode                    -- :: Binary a => ByteString -> a

    , module Data.Binary.PutM
    , module Data.Binary.GetM

  ) where

import Data.Binary.PutM
import Data.Binary.GetM

import Control.Monad
import Foreign

import qualified Data.ByteString as B

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Data.Char (ord, chr)
import Data.Array (Array)
import Data.Array.IArray
import Data.Array.Unboxed (UArray)

------------------------------------------------------------------------

encode :: Binary a => a -> ByteString
encode = runPutM . put

decode :: Binary a => ByteString -> a
decode = runGetM get

------------------------------------------------------------------------

class Binary t where
    put :: t -> PutM ()
    get :: GetM t

instance Binary () where
    put ()  = return ()
    get     = return ()

instance Binary Word8 where
    put     = putWord8
    get     = getWord8

-- 
-- todo, investigate bit fields for sequences of Bools
--
instance Binary Bool where
    put     = putWord8 . fromIntegral . fromEnum
    get     = liftM (toEnum . fromIntegral) getWord8

instance Binary Word16 where
    put     = putWord16be
    get     = getWord16be

instance Binary Word32 where
    put     = putWord32be
    get     = getWord32be

instance Binary Word64 where
    put     = putWord64be
    get     = getWord64be

instance Binary Int8 where
    put i   = put (fromIntegral i :: Word8)
    get     = fromIntegral `fmap` (get :: GetM Word8)

instance Binary Int16 where
    put i   = put (fromIntegral i :: Word16)
    get     = fromIntegral `fmap` (get :: GetM Word16)

instance Binary Int32 where
    put i   = put (fromIntegral i :: Word32)
    get     = fromIntegral `fmap` (get :: GetM Word32)

instance Binary Int64 where
    put i   = put (fromIntegral i :: Word64)
    get     = fromIntegral `fmap` (get :: GetM Word64)

instance Binary Int where
    put i   = put (fromIntegral i :: Int32)
    get     = fromIntegral `fmap` (get :: GetM Int32)

-- TODO Integer

-- TODO profile, benchmark and test this instance
instance Binary Char where
    put a | c <= 0x7f     = put (fromIntegral c :: Word8)
          | c <= 0x7ff    = do put (0xc0 .|. y)
                               put (0x80 .|. z)
          | c <= 0xffff   = do put (0xe0 .|. x)
                               put (0x80 .|. y)
                               put (0x80 .|. z)
          | c <= 0x10ffff = do put (0xf0 .|. w)
                               put (0x80 .|. x)
                               put (0x80 .|. y)
                               put (0x80 .|. z)
          | otherwise     = error "Not a valid Unicode code point"
     where
        c = ord a
        z, y, x, w :: Word8
        z = fromIntegral (c           .&. 0x3f)
        y = fromIntegral (shiftR c 6  .&. 0x3f)
        x = fromIntegral (shiftR c 12 .&. 0x3f)
        w = fromIntegral (shiftR c 18 .&. 0x7)

    get = do
        let getByte = fmap (fromIntegral :: Word8 -> Int) get
            shiftL6 = flip shiftL 6 :: Int -> Int
        w <- getByte
        r <- case () of
                _ | w < 0x80  -> return w
                  | w < 0xe0  -> do
                                    x <- fmap (xor 0x80) getByte
                                    return (x .|. shiftL6 (xor 0xc0 w))
                  | w < 0xf0  -> do
                                    x <- fmap (xor 0x80) getByte
                                    y <- fmap (xor 0x80) getByte
                                    return (y .|. shiftL6 (x .|. shiftL6
                                            (xor 0xe0 w)))
                  | otherwise -> do
                                x <- fmap (xor 0x80) getByte
                                y <- fmap (xor 0x80) getByte
                                z <- fmap (xor 0x80) getByte
                                return (z .|. shiftL6 (y .|. shiftL6
                                        (x .|. shiftL6 (xor 0x80 w))))
        return $! chr r

instance Binary a => Binary [a] where
    put l  = do
        put (length l)
        mapM_ put l
    get    = do
        n <- get :: GetM Int
        replicateM n get

instance (Binary a, Binary b) => Binary (Either a b) where
    put (Left  a) = putWord8 0 >> put a
    put (Right b) = putWord8 1 >> put b
    get = do
        w <- getWord8
        case w of
            0 -> liftM Left  get
            _ -> liftM Right get

instance (Binary a) => Binary (Maybe a) where
    put Nothing  = putWord8 0
    put (Just x) = putWord8 1 >> put x
    get = do
        w <- getWord8
        case w of
            0 -> return Nothing
            _ -> fmap Just get

instance (Binary a, Binary b) => Binary (a,b) where
    put (a,b) = put a >> put b
    get       = do a <- get
                   b <- get
                   return (a,b)

instance (Binary a, Binary b, Binary c) => Binary (a,b,c) where
    put (a,b,c) = put a >> put b >> put c
    get = do
       a <- get
       b <- get
       c <- get
       return (a,b,c)

instance (Binary a, Binary b, Binary c, Binary d) => Binary (a,b,c,d) where
    put (a,b,c,d) = put a >> put b >> put c >> put d
    get = do
        a <- get
        b <- get
        c <- get
        d <- get
        return (a,b,c,d)

instance Binary B.ByteString where
    put bs = do
        put (B.length bs)
        putByteString bs
    get = do
        len <- get
        getByteString len

-- 
-- needs the newtyped' bytestring
-- 
instance Binary ByteString where
    put bs = do
        put (L.length bs)
        putLazyByteString bs
    get = do
        len <- get
        getLazyByteString len

instance (Ord a, Binary a) => Binary (Set.Set a) where
    put = put . Set.toAscList
    get = fmap Set.fromDistinctAscList get

instance (Ord k, Binary k, Binary e) => Binary (Map.Map k e) where
    put = put . Map.toAscList
    get = fmap Map.fromDistinctAscList get

instance Binary IntSet.IntSet where
    put = put . IntSet.toAscList
    get = fmap IntSet.fromDistinctAscList get

instance (Binary e) => Binary (IntMap.IntMap e) where
    put = put . IntMap.toAscList
    get = fmap IntMap.fromDistinctAscList get

instance (Binary i, Ix i, Binary e) => Binary (Array i e) where
    put a = do
        put (bounds a)
        put $ elems a
    get = do
        bs <- get
        es <- get
        return (listArray bs es)

-- todo handle UArray i Bool specially?
--
-- N.B.
--
--  Non type-variable argument in the constraint: IArray UArray e
--  (Use -fglasgow-exts to permit this)
--
instance (Binary i, Ix i, Binary e, IArray UArray e) => Binary (UArray i e) where
    put a = do
        put (bounds a)
        put $ elems a
    get = do
        bs <- get
        es <- get
        return (listArray bs es)
