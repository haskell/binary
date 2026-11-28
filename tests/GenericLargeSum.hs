-- Compiled with -O0 because GSum131072 expands to a 2^17-leaf (:+:) type tree.
-- Under -O1/-O2, GHC inlines and simplifies the resulting 131,072-branch Core
-- tree, consuming ~30 GiB of RAM and OOM-killing CI runners (both 64-bit and
-- 32-bit i386). The library under test (Data.Binary.Generic) is still compiled
-- with full optimization.
{-# OPTIONS_GHC -O0 #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
module GenericLargeSum
  ( prop_Generic_Word16_and_Word32_sums
  ) where

import qualified Data.ByteString.Lazy as L
import           Data.Word            (Word16, Word32)
import           GHC.Generics         ((:+:)(..), C1, M1(..), U1(..), Meta(..), FixityI(..))

import           Data.Binary
import           Data.Binary.Get      (runGet, runGetOrFail)
import           Data.Binary.Put      (runPut, runPutM)

type GSum1      = C1 ('MetaCons "C" 'PrefixI 'False) U1
type GSum2      = GSum1 :+: GSum1
type GSum4      = GSum2 :+: GSum2
type GSum8      = GSum4 :+: GSum4
type GSum16     = GSum8 :+: GSum8
type GSum32     = GSum16 :+: GSum16
type GSum64     = GSum32 :+: GSum32
type GSum128    = GSum64 :+: GSum64
type GSum256    = GSum128 :+: GSum128
type GSum512    = GSum256 :+: GSum256
type GSum1024   = GSum512 :+: GSum512
type GSum2048   = GSum1024 :+: GSum1024
type GSum4096   = GSum2048 :+: GSum2048
type GSum8192   = GSum4096 :+: GSum4096
type GSum16384  = GSum8192 :+: GSum8192
type GSum32768  = GSum16384 :+: GSum16384
type GSum65536  = GSum32768 :+: GSum32768
type GSum131072 = GSum65536 :+: GSum65536

prop_Generic_Word16_and_Word32_sums :: Bool
prop_Generic_Word16_and_Word32_sums =
    let -- Force U1 unit result
        ((), u1bs) = runPutM (gput (U1 :: U1 ()))
        -- 512 constructors (balanced) -> Word16 tag (2 bytes)
        first512 :: GSum512 ()
        first512 = L1 (L1 (L1 (L1 (L1 (L1 (L1 (L1 (L1 (M1 U1)))))))))
        last512 :: GSum512 ()
        last512  = R1 (R1 (R1 (R1 (R1 (R1 (R1 (R1 (R1 (M1 U1)))))))))
        bsFirst512 = runPut (gput first512)
        bsLast512  = runPut (gput last512)
        -- 131072 constructors (balanced) -> Word32 tag (4 bytes)
        first131072 :: GSum131072 ()
        first131072 = L1 (L1 (L1 (L1 (L1 (L1 (L1 (L1 (L1 (L1 (L1 (L1 (L1 (L1 (L1 (L1 (L1 (M1 U1)))))))))))))))))
        last131072 :: GSum131072 ()
        last131072  = R1 (R1 (R1 (R1 (R1 (R1 (R1 (R1 (R1 (R1 (R1 (R1 (R1 (R1 (R1 (R1 (R1 (M1 U1)))))))))))))))))
        bsFirst131072 = runPut (gput first131072)
        bsLast131072  = runPut (gput last131072)
        isFail (Left (_, _, msg)) = not (null msg)
        isFail (Right _)          = False
    in L.null u1bs
    && L.length bsFirst512 == 2
    && bsFirst512 == encode (0 :: Word16)
    && bsLast512  == encode (511 :: Word16)
    && runGet gget bsFirst512 == first512
    && runGet gget bsLast512  == last512
    && isFail (runGetOrFail (gget :: Get (GSum512 ())) (encode (512 :: Word16)))
    && L.length bsFirst131072 == 4
    && bsFirst131072 == encode (0 :: Word32)
    && bsLast131072  == encode (131071 :: Word32)
    && runGet gget bsFirst131072 == first131072
    && runGet gget bsLast131072  == last131072
    && isFail (runGetOrFail (gget :: Get (GSum131072 ())) (encode (131072 :: Word32)))
