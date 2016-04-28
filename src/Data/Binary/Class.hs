{-# LANGUAGE CPP, FlexibleContexts #-}
#if __GLASGOW_HASKELL__ >= 701 && __GLASGOW_HASKELL__ != 702
{-# LANGUAGE Safe #-}
#endif
#ifdef GENERICS
{-# LANGUAGE DefaultSignatures #-}
#endif

#if MIN_VERSION_base(4,8,0)
#define HAS_NATURAL
#define HAS_VOID
#endif

#if MIN_VERSION_base(4,7,0)
#define HAS_FIXED_CONSTRUCTOR
#endif

#if __GLASGOW_HASKELL__ >= 704
#define HAS_GHC_FINGERPRINT
#endif

#ifndef HAS_FIXED_CONSTRUCTOR
{-# LANGUAGE ScopedTypeVariables #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Binary.Class
-- Copyright   : Lennart Kolmodin
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Lennart Kolmodin <kolmodin@gmail.com>
-- Stability   : unstable
-- Portability : portable to Hugs and GHC. Requires the FFI and some flexible instances
--
-- Typeclass and instances for binary serialization.
--
-----------------------------------------------------------------------------

module Data.Binary.Class (

    -- * The Binary class
      Binary(..)

#ifdef GENERICS
    -- * Support for generics
    , GBinaryGet(..)
    , GBinaryPut(..)
#endif

    ) where

import Data.Word
import Data.Bits
import Data.Int
import Data.Complex (Complex(..))
#ifdef HAS_VOID
import Data.Void
#endif

import Data.Binary.Put
import Data.Binary.Get
import Data.Binary.Internal ((<>))

#if ! MIN_VERSION_base(4,8,0)
import Control.Applicative
import Data.Monoid (mempty)
#endif
import Control.Monad

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Builder.Prim as Prim

import Data.List    (unfoldr, foldl')

-- And needed for the instances:
import qualified Data.ByteString as B
#if MIN_VERSION_bytestring(0,10,4)
import qualified Data.ByteString.Short as BS
#endif
import qualified Data.Map        as Map
import qualified Data.Set        as Set
import qualified Data.IntMap     as IntMap
import qualified Data.IntSet     as IntSet
import qualified Data.Ratio      as R

import qualified Data.Tree as T

import Data.Array.Unboxed

#ifdef GENERICS
import GHC.Generics
#endif

#ifdef HAS_NATURAL
import Numeric.Natural
#endif

import qualified Data.Fixed as Fixed

--
-- This isn't available in older Hugs or older GHC
--
#if __GLASGOW_HASKELL__ >= 606
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Fold
#endif

#ifdef HAS_GHC_FINGERPRINT
import GHC.Fingerprint
#endif

import Data.Version (Version(..))

------------------------------------------------------------------------

#ifdef GENERICS
-- Factored into two classes because this makes GHC optimize the
-- instances faster.  This doesn't matter for builds of binary,
-- but it matters a lot for end-users who write 'instance Binary T'.
-- See also: https://ghc.haskell.org/trac/ghc/ticket/9630
class GBinaryPut f where
    gput :: f t -> Put

class GBinaryGet f where
    gget :: Get (f t)
#endif

-- | The 'Binary' class provides 'put' and 'get', methods to encode and
-- decode a Haskell value to a lazy 'ByteString'. It mirrors the 'Read' and
-- 'Show' classes for textual representation of Haskell types, and is
-- suitable for serialising Haskell values to disk, over the network.
--
-- For decoding and generating simple external binary formats (e.g. C
-- structures), Binary may be used, but in general is not suitable
-- for complex protocols. Instead use the 'Put' and 'Get' primitives
-- directly.
--
-- Instances of Binary should satisfy the following property:
--
-- > decode . encode == id
--
-- That is, the 'get' and 'put' methods should be the inverse of each
-- other. A range of instances are provided for basic Haskell types.
--
class Binary t where
    -- | Encode a value in the Put monad.
    put :: t -> Put
    -- | Decode a value in the Get monad
    get :: Get t

    -- | Encode a list of values in the Put monad.
    -- The default implementation may be overridden to be more efficient
    -- but must still have the same encoding format.
    putList :: [t] -> Put
    putList = defaultPutList

#ifdef GENERICS
    default put :: (Generic t, GBinaryPut (Rep t)) => t -> Put
    put = gput . from

    default get :: (Generic t, GBinaryGet (Rep t)) => Get t
    get = to `fmap` gget
#endif

{-# INLINE defaultPutList #-}
defaultPutList :: Binary a => [a] -> Put
defaultPutList xs = put (length xs) <> mapM_ put xs

------------------------------------------------------------------------
-- Simple instances

#ifdef HAS_VOID
-- Void never gets written nor reconstructed since it's impossible to have a
-- value of that type

-- | /Since: 0.8.0.0/
instance Binary Void where
    put     = absurd
    get     = mzero
#endif

-- The () type need never be written to disk: values of singleton type
-- can be reconstructed from the type alone
instance Binary () where
    put ()  = mempty
    get     = return ()

-- Bools are encoded as a byte in the range 0 .. 1
instance Binary Bool where
    put     = putWord8 . fromIntegral . fromEnum
    get     = getWord8 >>= toBool
      where
        toBool 0 = return False
        toBool 1 = return True
        toBool c = fail ("Could not map value " ++ show c ++ " to Bool")

-- Values of type 'Ordering' are encoded as a byte in the range 0 .. 2
instance Binary Ordering where
    put     = putWord8 . fromIntegral . fromEnum
    get     = getWord8 >>= toOrd
      where
        toOrd 0 = return LT
        toOrd 1 = return EQ
        toOrd 2 = return GT
        toOrd c = fail ("Could not map value " ++ show c ++ " to Ordering")

------------------------------------------------------------------------
-- Words and Ints

-- Words8s are written as bytes
instance Binary Word8 where
    put     = putWord8
    {-# INLINE putList #-}
    putList xs =
        put (length xs)
        <> putBuilder (Prim.primMapListFixed Prim.word8 xs)
    get     = getWord8

-- Words16s are written as 2 bytes in big-endian (network) order
instance Binary Word16 where
    put     = putWord16be
    {-# INLINE putList #-}
    putList xs =
        put (length xs)
        <> putBuilder (Prim.primMapListFixed Prim.word16BE xs)
    get     = getWord16be

-- Words32s are written as 4 bytes in big-endian (network) order
instance Binary Word32 where
    put     = putWord32be
    {-# INLINE putList #-}
    putList xs =
        put (length xs)
        <> putBuilder (Prim.primMapListFixed Prim.word32BE xs)
    get     = getWord32be

-- Words64s are written as 8 bytes in big-endian (network) order
instance Binary Word64 where
    put     = putWord64be
    {-# INLINE putList #-}
    putList xs =
        put (length xs)
        <> putBuilder (Prim.primMapListFixed Prim.word64BE xs)
    get     = getWord64be

-- Int8s are written as a single byte.
instance Binary Int8 where
    put     = putInt8
    {-# INLINE putList #-}
    putList xs =
        put (length xs)
        <> putBuilder (Prim.primMapListFixed Prim.int8 xs)
    get     = getInt8

-- Int16s are written as a 2 bytes in big endian format
instance Binary Int16 where
    put     = putInt16be
    {-# INLINE putList #-}
    putList xs =
        put (length xs)
        <> putBuilder (Prim.primMapListFixed Prim.int16BE xs)
    get     = getInt16be

-- Int32s are written as a 4 bytes in big endian format
instance Binary Int32 where
    put     = putInt32be
    {-# INLINE putList #-}
    putList xs =
        put (length xs)
        <> putBuilder (Prim.primMapListFixed Prim.int32BE xs)
    get     = getInt32be

-- Int64s are written as a 8 bytes in big endian format
instance Binary Int64 where
    put     = putInt64be
    {-# INLINE putList #-}
    putList xs =
        put (length xs)
        <> putBuilder (Prim.primMapListFixed Prim.int64BE xs)
    get     = getInt64be

------------------------------------------------------------------------

-- Words are are written as Word64s, that is, 8 bytes in big endian format
instance Binary Word where
    put     = putWord64be . fromIntegral
    {-# INLINE putList #-}
    putList xs =
        put (length xs)
        <> putBuilder (Prim.primMapListFixed Prim.word64BE (map fromIntegral xs))
    get     = liftM fromIntegral getWord64be

-- Ints are are written as Int64s, that is, 8 bytes in big endian format
instance Binary Int where
    put     = putInt64be . fromIntegral
    {-# INLINE putList #-}
    putList xs =
        put (length xs)
        <> putBuilder (Prim.primMapListFixed Prim.int64BE (map fromIntegral xs))
    get     = liftM fromIntegral getInt64be

------------------------------------------------------------------------
--
-- Portable, and pretty efficient, serialisation of Integer
--

-- Fixed-size type for a subset of Integer
type SmallInt = Int32

-- Integers are encoded in two ways: if they fit inside a SmallInt,
-- they're written as a byte tag, and that value.  If the Integer value
-- is too large to fit in a SmallInt, it is written as a byte array,
-- along with a sign and length field.

instance Binary Integer where

    {-# INLINE put #-}
    put n | n >= lo && n <= hi =
        putBuilder (Prim.primFixed (Prim.word8 Prim.>*< Prim.int32BE) (0, fromIntegral n))
     where
        lo = fromIntegral (minBound :: SmallInt) :: Integer
        hi = fromIntegral (maxBound :: SmallInt) :: Integer

    put n =
        putWord8 1
        <> put sign
        <> put (unroll (abs n))         -- unroll the bytes
     where
        sign = fromIntegral (signum n) :: Word8

    {-# INLINE get #-}
    get = do
        tag <- get :: Get Word8
        case tag of
            0 -> liftM fromIntegral (get :: Get SmallInt)
            _ -> do sign  <- get
                    bytes <- get
                    let v = roll bytes
                    return $! if sign == (1 :: Word8) then v else - v

-- | /Since: 0.8.0.0/
#ifdef HAS_FIXED_CONSTRUCTOR
instance Binary (Fixed.Fixed a) where
  put (Fixed.MkFixed a) = put a
  get = Fixed.MkFixed `liftM` get
#else
instance forall a. Fixed.HasResolution a => Binary (Fixed.Fixed a) where
  -- Using undefined :: Maybe a as a proxy, as Data.Proxy is introduced only in base-4.7
  put x = put (truncate (x * fromInteger (Fixed.resolution (undefined :: Maybe a))) :: Integer)
  get = (\x -> fromInteger x / fromInteger (Fixed.resolution (undefined :: Maybe a))) `liftM` get
#endif

--
-- Fold and unfold an Integer to and from a list of its bytes
--
unroll :: (Integral a, Bits a) => a -> [Word8]
unroll = unfoldr step
  where
    step 0 = Nothing
    step i = Just (fromIntegral i, i `shiftR` 8)

roll :: (Integral a, Bits a) => [Word8] -> a
roll   = foldl' unstep 0 . reverse
  where
    unstep a b = a `shiftL` 8 .|. fromIntegral b

#ifdef HAS_NATURAL
-- Fixed-size type for a subset of Natural
type NaturalWord = Word64

-- | /Since: 0.7.3.0/
instance Binary Natural where
    {-# INLINE put #-}
    put n | n <= hi =
        putWord8 0
        <> put (fromIntegral n :: NaturalWord)  -- fast path
     where
        hi = fromIntegral (maxBound :: NaturalWord) :: Natural

    put n =
        putWord8 1
        <> put (unroll (abs n))         -- unroll the bytes

    {-# INLINE get #-}
    get = do
        tag <- get :: Get Word8
        case tag of
            0 -> liftM fromIntegral (get :: Get NaturalWord)
            _ -> do bytes <- get
                    return $! roll bytes
#endif

{-

--
-- An efficient, raw serialisation for Integer (GHC only)
--

-- TODO  This instance is not architecture portable.  GMP stores numbers as
-- arrays of machine sized words, so the byte format is not portable across
-- architectures with different endianness and word size.

import Data.ByteString.Base (toForeignPtr,unsafePackAddress, memcpy)
import GHC.Base     hiding (ord, chr)
import GHC.Prim
import GHC.Ptr (Ptr(..))
import GHC.IOBase (IO(..))

instance Binary Integer where
    put (S# i)    = putWord8 0 >> put (I# i)
    put (J# s ba) = do
        putWord8 1
        put (I# s)
        put (BA ba)

    get = do
        b <- getWord8
        case b of
            0 -> do (I# i#) <- get
                    return (S# i#)
            _ -> do (I# s#) <- get
                    (BA a#) <- get
                    return (J# s# a#)

instance Binary ByteArray where

    -- Pretty safe.
    put (BA ba) =
        let sz   = sizeofByteArray# ba   -- (primitive) in *bytes*
            addr = byteArrayContents# ba
            bs   = unsafePackAddress (I# sz) addr
        in put bs   -- write as a ByteString. easy, yay!

    -- Pretty scary. Should be quick though
    get = do
        (fp, off, n@(I# sz)) <- liftM toForeignPtr get      -- so decode a ByteString
        assert (off == 0) $ return $ unsafePerformIO $ do
            (MBA arr) <- newByteArray sz                    -- and copy it into a ByteArray#
            let to = byteArrayContents# (unsafeCoerce# arr) -- urk, is this safe?
            withForeignPtr fp $ \from -> memcpy (Ptr to) from (fromIntegral n)
            freezeByteArray arr

-- wrapper for ByteArray#
data ByteArray = BA  {-# UNPACK #-} !ByteArray#
data MBA       = MBA {-# UNPACK #-} !(MutableByteArray# RealWorld)

newByteArray :: Int# -> IO MBA
newByteArray sz = IO $ \s ->
  case newPinnedByteArray# sz s of { (# s', arr #) ->
  (# s', MBA arr #) }

freezeByteArray :: MutableByteArray# RealWorld -> IO ByteArray
freezeByteArray arr = IO $ \s ->
  case unsafeFreezeByteArray# arr s of { (# s', arr' #) ->
  (# s', BA arr' #) }

-}

instance (Binary a,Integral a) => Binary (R.Ratio a) where
    put r = put (R.numerator r) <> put (R.denominator r)
    get = liftM2 (R.%) get get

instance ( Binary a
#if !(MIN_VERSION_base(4,4,0))
         , RealFloat a
#endif
         ) => Binary (Complex a) where
    {-# INLINE put #-}
    put (r :+ i) = put (r, i)
    {-# INLINE get #-}
    get = (\(r,i) -> r :+ i) <$> get

------------------------------------------------------------------------

-- Char is serialised as UTF-8
instance Binary Char where
    put = putCharUtf8
    putList str = put (length str) <> putStringUtf8 str
    get = do
        let getByte = liftM (fromIntegral :: Word8 -> Int) get
            shiftL6 = flip shiftL 6 :: Int -> Int
        w <- getByte
        r <- case () of
                _ | w < 0x80  -> return w
                  | w < 0xe0  -> do
                                    x <- liftM (xor 0x80) getByte
                                    return (x .|. shiftL6 (xor 0xc0 w))
                  | w < 0xf0  -> do
                                    x <- liftM (xor 0x80) getByte
                                    y <- liftM (xor 0x80) getByte
                                    return (y .|. shiftL6 (x .|. shiftL6
                                            (xor 0xe0 w)))
                  | otherwise -> do
                                x <- liftM (xor 0x80) getByte
                                y <- liftM (xor 0x80) getByte
                                z <- liftM (xor 0x80) getByte
                                return (z .|. shiftL6 (y .|. shiftL6
                                        (x .|. shiftL6 (xor 0xf0 w))))
        getChr r
      where
        getChr w
          | w <= 0x10ffff = return $! toEnum $ fromEnum w
          | otherwise = fail "Not a valid Unicode code point!"

------------------------------------------------------------------------
-- Instances for the first few tuples

instance (Binary a, Binary b) => Binary (a,b) where
    put (a,b)           = put a <> put b
    get                 = liftM2 (,) get get

instance (Binary a, Binary b, Binary c) => Binary (a,b,c) where
    put (a,b,c)         = put a <> put b <> put c
    get                 = liftM3 (,,) get get get

instance (Binary a, Binary b, Binary c, Binary d) => Binary (a,b,c,d) where
    put (a,b,c,d)       = put a <> put b <> put c <> put d
    get                 = liftM4 (,,,) get get get get

instance (Binary a, Binary b, Binary c, Binary d, Binary e) => Binary (a,b,c,d,e) where
    put (a,b,c,d,e)     = put a <> put b <> put c <> put d <> put e
    get                 = liftM5 (,,,,) get get get get get

--
-- and now just recurse:
--

instance (Binary a, Binary b, Binary c, Binary d, Binary e, Binary f)
        => Binary (a,b,c,d,e,f) where
    put (a,b,c,d,e,f)   = put (a,(b,c,d,e,f))
    get                 = do (a,(b,c,d,e,f)) <- get ; return (a,b,c,d,e,f)

instance (Binary a, Binary b, Binary c, Binary d, Binary e, Binary f, Binary g)
        => Binary (a,b,c,d,e,f,g) where
    put (a,b,c,d,e,f,g) = put (a,(b,c,d,e,f,g))
    get                 = do (a,(b,c,d,e,f,g)) <- get ; return (a,b,c,d,e,f,g)

instance (Binary a, Binary b, Binary c, Binary d, Binary e,
          Binary f, Binary g, Binary h)
        => Binary (a,b,c,d,e,f,g,h) where
    put (a,b,c,d,e,f,g,h) = put (a,(b,c,d,e,f,g,h))
    get                   = do (a,(b,c,d,e,f,g,h)) <- get ; return (a,b,c,d,e,f,g,h)

instance (Binary a, Binary b, Binary c, Binary d, Binary e,
          Binary f, Binary g, Binary h, Binary i)
        => Binary (a,b,c,d,e,f,g,h,i) where
    put (a,b,c,d,e,f,g,h,i) = put (a,(b,c,d,e,f,g,h,i))
    get                     = do (a,(b,c,d,e,f,g,h,i)) <- get ; return (a,b,c,d,e,f,g,h,i)

instance (Binary a, Binary b, Binary c, Binary d, Binary e,
          Binary f, Binary g, Binary h, Binary i, Binary j)
        => Binary (a,b,c,d,e,f,g,h,i,j) where
    put (a,b,c,d,e,f,g,h,i,j) = put (a,(b,c,d,e,f,g,h,i,j))
    get                       = do (a,(b,c,d,e,f,g,h,i,j)) <- get ; return (a,b,c,d,e,f,g,h,i,j)

------------------------------------------------------------------------
-- Container types

instance Binary a => Binary [a] where
    put = putList
    get = do n <- get :: Get Int
             getMany n

-- | 'getMany n' get 'n' elements in order, without blowing the stack.
getMany :: Binary a => Int -> Get [a]
getMany n = go [] n
 where
    go xs 0 = return $! reverse xs
    go xs i = do x <- get
                 -- we must seq x to avoid stack overflows due to laziness in
                 -- (>>=)
                 x `seq` go (x:xs) (i-1)
{-# INLINE getMany #-}

instance (Binary a) => Binary (Maybe a) where
    put Nothing  = putWord8 0
    put (Just x) = putWord8 1 <> put x
    get = do
        w <- getWord8
        case w of
            0 -> return Nothing
            _ -> liftM Just get

instance (Binary a, Binary b) => Binary (Either a b) where
    put (Left  a) = putWord8 0 <> put a
    put (Right b) = putWord8 1 <> put b
    get = do
        w <- getWord8
        case w of
            0 -> liftM Left  get
            _ -> liftM Right get

------------------------------------------------------------------------
-- ByteStrings (have specially efficient instances)

instance Binary B.ByteString where
    put bs = put (B.length bs)
             <> putByteString bs
    get    = get >>= getByteString

--
-- Using old versions of fps, this is a type synonym, and non portable
--
-- Requires 'flexible instances'
--
instance Binary ByteString where
    put bs = put (fromIntegral (L.length bs) :: Int)
             <> putLazyByteString bs
    get    = get >>= getLazyByteString


#if MIN_VERSION_bytestring(0,10,4)
instance Binary BS.ShortByteString where
   put bs = put (BS.length bs)
            <> putShortByteString bs
   get = get >>= fmap BS.toShort . getByteString
#endif

------------------------------------------------------------------------
-- Maps and Sets

instance (Binary a) => Binary (Set.Set a) where
    put s = put (Set.size s) <> mapM_ put (Set.toAscList s)
    get   = liftM Set.fromDistinctAscList get

instance (Binary k, Binary e) => Binary (Map.Map k e) where
    put m = put (Map.size m) <> mapM_ put (Map.toAscList m)
    get   = liftM Map.fromDistinctAscList get

instance Binary IntSet.IntSet where
    put s = put (IntSet.size s) <> mapM_ put (IntSet.toAscList s)
    get   = liftM IntSet.fromDistinctAscList get

instance (Binary e) => Binary (IntMap.IntMap e) where
    put m = put (IntMap.size m) <> mapM_ put (IntMap.toAscList m)
    get   = liftM IntMap.fromDistinctAscList get

------------------------------------------------------------------------
-- Queues and Sequences

#if __GLASGOW_HASKELL__ >= 606
--
-- This is valid Hugs, but you need the most recent Hugs
--

instance (Binary e) => Binary (Seq.Seq e) where
    put s = put (Seq.length s) <> Fold.mapM_ put s
    get = do n <- get :: Get Int
             rep Seq.empty n get
      where rep xs 0 _ = return $! xs
            rep xs n g = xs `seq` n `seq` do
                           x <- g
                           rep (xs Seq.|> x) (n-1) g

#endif

------------------------------------------------------------------------
-- Floating point

instance Binary Double where
    put d = put (decodeFloat d)
    get   = do
        x <- get
        y <- get
        return $! encodeFloat x y

instance Binary Float where
    put f = put (decodeFloat f)
    get   =  do
        x <- get
        y <- get
        return $! encodeFloat x y

------------------------------------------------------------------------
-- Trees

instance (Binary e) => Binary (T.Tree e) where
    put (T.Node r s) = put r <> put s
    get = liftM2 T.Node get get

------------------------------------------------------------------------
-- Arrays

instance (Binary i, Ix i, Binary e) => Binary (Array i e) where
    put a =
        put (bounds a)
        <> put (rangeSize $ bounds a) -- write the length
        <> mapM_ put (elems a)        -- now the elems.
    get = do
        bs <- get
        n  <- get                  -- read the length
        xs <- getMany n            -- now the elems.
        return (listArray bs xs)

--
-- The IArray UArray e constraint is non portable. Requires flexible instances
--
instance (Binary i, Ix i, Binary e, IArray UArray e) => Binary (UArray i e) where
    put a =
        put (bounds a)
        <> put (rangeSize $ bounds a) -- now write the length
        <> mapM_ put (elems a)
    get = do
        bs <- get
        n  <- get
        xs <- getMany n
        return (listArray bs xs)

------------------------------------------------------------------------
-- Fingerprints

#ifdef HAS_GHC_FINGERPRINT
-- | /Since: 0.7.6.0/
instance Binary Fingerprint where
    put (Fingerprint x1 x2) = put x1 <> put x2
    get = do
        x1 <- get
        x2 <- get
        return $! Fingerprint x1 x2
#endif

------------------------------------------------------------------------
-- Version

-- | /Since: 0.8.0.0/
instance Binary Version where
    put (Version br tags) = put br <> put tags
    get = Version <$> get <*> get
