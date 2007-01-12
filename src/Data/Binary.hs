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

    -- * The Binary class
      Binary(..)

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

    -- * Lazy put and get
    , lazyPut
    , lazyGet

    ) where

import Data.Binary.Put
import Data.Binary.Get

import Control.Monad
import Foreign

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L

import Data.Char    (ord, chr)

-- and needed for the instances:
import qualified Data.ByteString as B
import qualified Data.Map        as Map
import qualified Data.Set        as Set
import qualified Data.IntMap     as IntMap
import qualified Data.IntSet     as IntSet

import Data.Array.Unboxed
import Data.List (unfoldr)
import qualified Data.Tree as T

import System.IO

--
-- GHC only instances for now
--
#if defined(__GLASGOW_HASKELL__)
import qualified Data.Sequence as Seq
#endif

--
-- For the Integer instance
--
#if defined(__GLASGOW_HASKELL__)
import Data.ByteString.Base (toForeignPtr,unsafePackAddress, memcpy)
import GHC.Num
import GHC.Base     hiding (ord, chr)
import GHC.Prim
import GHC.Ptr (Ptr(..))
import GHC.IOBase (IO(..))
#endif

------------------------------------------------------------------------

-- | The @Binary@ class provides 'put' and 'get', methods to encode and
-- decode a value to a lazy bytestring.
--
-- New instances for binary should have the following property:
--
-- > get . put == id
--
-- A range of instances are provided for basic Haskell types. To
-- serialise a custom type, an instance of Binary for that type is
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
class Binary t where
    -- | Encode a value in the Put monad.
    put :: t -> Put ()
    -- | Decode a value in the Get monad
    get :: Get t

------------------------------------------------------------------------
-- Wrappers to run the underlying monad

-- | Encode a value using binary serialisation to a lazy ByteString.
--
encode :: Binary a => a -> ByteString
encode = runPut . put

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

-- | Lazily reconstruct a value previously written to a file
--
-- This is just a convenience function, it's defined simply as:
--
-- > decodeFile f = return . decode =<< B.readFile f
--
-- So for example if you wanted to decompress as well, you could use:
--
-- > return . decode . decompress =<< B.readFile f
--
decodeFile :: Binary a => FilePath -> IO a
decodeFile f = liftM decode (L.readFile f)

------------------------------------------------------------------------
-- Lazy put and get

lazyPut :: (Binary a) => a -> Put ()
lazyPut a = put (encode a)

lazyGet :: (Binary a) => Get a
lazyGet = fmap decode get

------------------------------------------------------------------------
-- Simple instances

instance Binary () where
    put ()  = return ()
    get     = return ()

instance Binary Ordering where
    put = putWord8 . fromIntegral . fromEnum
    get = liftM (toEnum . fromIntegral) getWord8

------------------------------------------------------------------------
-- Words and Ints

instance Binary Word8 where
    put     = putWord8
    get     = getWord8

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
    get     = liftM fromIntegral (get :: Get Word8)

instance Binary Int16 where
    put i   = put (fromIntegral i :: Word16)
    get     = liftM fromIntegral (get :: Get Word16)

instance Binary Int32 where
    put i   = put (fromIntegral i :: Word32)
    get     = liftM fromIntegral (get :: Get Word32)

instance Binary Int64 where
    put i   = put (fromIntegral i :: Word64)
    get     = liftM fromIntegral (get :: Get Word64)

-- XXX Not portable to 64 bit machine yet.
instance Binary Int where
    put i   = put (fromIntegral i :: Int32)
    get     = liftM fromIntegral (get :: Get Int32)

------------------------------------------------------------------------
-- Old style Integer instance

--
-- Bogus instance. Rewrite tomorrow.
--

#if defined(__GLASGOW_HASKELL__)

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
data ByteArray = BA ByteArray#
data MBA       = MBA (MutableByteArray# RealWorld)

newByteArray :: Int# -> IO MBA
newByteArray sz = IO $ \s ->
  case newPinnedByteArray# sz s of { (# s', arr #) ->
  (# s', MBA arr #) }

freezeByteArray :: MutableByteArray# RealWorld -> IO ByteArray
freezeByteArray arr = IO $ \s ->
  case unsafeFreezeByteArray# arr s of { (# s', arr' #) ->
  (# s', BA arr' #) }

#endif

------------------------------------------------------------------------
-- Char

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
        return $! chr r

------------------------------------------------------------------------
-- Instances for the first few tuples

instance (Binary a, Binary b) => Binary (a,b) where
    put (a,b) = put a >> put b
    get       = do a <- get
                   b <- get
                   return (a,b)

--
-- And then the recursive cases
--

instance (Binary a, Binary b, Binary c) => Binary (a,b,c) where
    put (a,b,c)         = put (a, (b,c))
    get                 = do (a,(b,c)) <- get ; return (a,b,c)

instance (Binary a, Binary b, Binary c, Binary d) => Binary (a,b,c,d) where
    put (a,b,c,d)       = put (a,(b,c,d))
    get                 = do (a,(b,c,d)) <- get ; return (a,b,c,d)

instance (Binary a, Binary b, Binary c, Binary d, Binary e) => Binary (a,b,c,d,e) where
    put (a,b,c,d,e)     = put (a,(b,c,d,e))
    get                 = do (a,(b,c,d,e)) <- get ; return (a,b,c,d,e)

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
    put l  = put (length l) >> mapM_ put l
    get    = do n <- get :: Get Int
                replicateM n get

instance (Binary a) => Binary (Maybe a) where
    put Nothing  = putWord8 0
    put (Just x) = putWord8 1 >> put x
    get = do
        w <- getWord8
        case w of
            0 -> return Nothing
            _ -> liftM Just get

instance (Binary a, Binary b) => Binary (Either a b) where
    put (Left  a) = putWord8 0 >> put a
    put (Right b) = putWord8 1 >> put b
    get = do
        w <- getWord8
        case w of
            0 -> liftM Left  get
            _ -> liftM Right get

------------------------------------------------------------------------
-- ByteStrings (have specially efficient instances)

instance Binary B.ByteString where
    put bs = do put (B.length bs)
                putByteString bs
    get    = get >>= getByteString

instance Binary ByteString where
    put bs = do put (fromIntegral (L.length bs) :: Int)
                putLazyByteString bs
    get    = get >>= getLazyByteString

------------------------------------------------------------------------
-- Maps and Sets

instance (Ord a, Binary a) => Binary (Set.Set a) where
    put = put . Set.toAscList
    get = liftM Set.fromDistinctAscList get

instance (Ord k, Binary k, Binary e) => Binary (Map.Map k e) where
    put = put . Map.toAscList
    get = liftM Map.fromDistinctAscList get

instance Binary IntSet.IntSet where
    put = put . IntSet.toAscList
    get = liftM IntSet.fromDistinctAscList get

instance (Binary e) => Binary (IntMap.IntMap e) where
    put = put . IntMap.toAscList
    get = liftM IntMap.fromDistinctAscList get

------------------------------------------------------------------------
-- Queues and Sequences

#if defined(__GLASGOW_HASKELL__)
instance (Binary e) => Binary (Seq.Seq e) where
    -- any better way to do this?
    put s = put . flip unfoldr s $ \sq ->
        case Seq.viewl sq of
            Seq.EmptyL -> Nothing
            (Seq.:<) e sq' -> Just (e,sq')
    get = fmap Seq.fromList get
#endif

------------------------------------------------------------------------
-- Trees

instance (Binary e) => Binary (T.Tree e) where
    put (T.Node r s) = put r >> put s
    get = liftM2 T.Node get get

------------------------------------------------------------------------
-- Arrays

instance (Binary i, Ix i, Binary e) => Binary (Array i e) where
    put a = do
        put (bounds a)
        put (elems a)
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
        put (elems a)
    get = do
        bs <- get
        es <- get
        return (listArray bs es)
