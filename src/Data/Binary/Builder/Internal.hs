{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      : Data.Binary.Builder.Internal
-- Copyright   : Lennart Kolmodin, Ross Paterson
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Lennart Kolmodin <kolmodin@gmail.com>
-- Stability   : experimental
-- Portability : portable to Hugs and GHC
--
-- A module containing semi-public 'Builder' internals that exposes
-- low level construction functions.  Modules which extend the
-- 'Builder' system will need to use this module while ideally most
-- users will be able to make do with the public interface modules.
--
-----------------------------------------------------------------------------

module Data.Binary.Builder.Internal (
    -- * Low-level construction of Builders
      writeN
    , writeAtMost
    ) where

import Data.Binary.Builder.Base
