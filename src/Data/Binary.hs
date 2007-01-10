-----------------------------------------------------------------------------
-- |
-- Module      :
-- Copyright   :  (c)
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer  :
-- Stability   :  stable
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Data.Binary (
      module Data.Binary.EncM
    , module Data.Binary.DecM
    , put
    , get
  ) where

import Data.Binary.EncM
import Data.Binary.DecM

import Control.Monad
import Foreign

class Binary t where
    put :: t -> EncM ()
    get :: DecM t

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
    get     = get >>= \(w::Word8) -> return $! fromIntegral w

instance Binary Int16 where
    put i   = put (fromIntegral i :: Word16)
    get     = get >>= \(w::Word16) -> return $! fromIntegral w

instance Binary Int32 where
    put i   = put (fromIntegral i :: Word32)
    get     = get >>= \(w::Word32) -> return $! fromIntegral w

instance Binary Int64 where
    put i   = put (fromIntegral i :: Word64)
    get     = get >>= \(w::Word64) -> return $! fromIntegral w

instance Binary Int where
    put i   = put (fromIntegral i :: Int32)
    get     = get >>= \(i::Int32) -> return $! fromIntegral i

-- TODO Integer

instance Binary a => Binary [a] where
    put l  = do
        put (length l)
        mapM_ put l
    get    = do
        (n :: Int) <- get
        replicateM n get

instance (Binary a, Binary b) => Binary (Either a b) where
    put (Left  a) = putWord8 0 >> put a
    put (Right b) = putWord8 1 >> put b
    get = do
        w <- getWord8
        case w of
            0 -> liftM Left  get
            _ -> liftM Right get

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

