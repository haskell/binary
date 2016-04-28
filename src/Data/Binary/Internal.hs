{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

module Data.Binary.Internal
 ( accursedUnutterablePerformIO
 , (<>)
 ) where

#if MIN_VERSION_bytestring(0,10,6)
import Data.ByteString.Internal( accursedUnutterablePerformIO )
#else
import Data.ByteString.Internal( inlinePerformIO )
#endif

#if MIN_VERSION_base(4,5,0)
import Data.Monoid ((<>))
#else
import Data.Monoid (Monoid(mappend))
#endif

#if !(MIN_VERSION_bytestring(0,10,6))
{-# INLINE accursedUnutterablePerformIO #-}
-- | You must be truly desperate to come to me for help.
accursedUnutterablePerformIO :: IO a -> a
accursedUnutterablePerformIO = inlinePerformIO
#endif

#if !(MIN_VERSION_base(4,5,0))
-- Defined for compatibility.

-- | An infix synonym for 'mappend'.
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
{-# INLINE (<>) #-}
#endif
