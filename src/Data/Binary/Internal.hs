{-# LANGUAGE CPP #-}

module Data.Binary.Internal
 ( accursedUnutterablePerformIO ) where

#if defined(__MHS__)
import System.IO.Unsafe(unsafePerformIO)

accursedUnutterablePerformIO :: IO a -> a
accursedUnutterablePerformIO = unsafePerformIO
#elif MIN_VERSION_bytestring(0,10,6)
import Data.ByteString.Internal( accursedUnutterablePerformIO )
#else
import Data.ByteString.Internal( inlinePerformIO )

{-# INLINE accursedUnutterablePerformIO #-}
-- | You must be truly desperate to come to me for help.
accursedUnutterablePerformIO :: IO a -> a
accursedUnutterablePerformIO = inlinePerformIO
#endif
