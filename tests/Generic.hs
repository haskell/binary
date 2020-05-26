{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Generic where

import           Data.Binary
import           GHC.Generics
import           Test.QuickCheck

data Generic256 =
       C00 | C01 | C02 | C03 | C04 | C05 | C06 | C07 | C08 | C09 | C0a | C0b | C0c | C0d | C0e | C0f
     | C10 | C11 | C12 | C13 | C14 | C15 | C16 | C17 | C18 | C19 | C1a | C1b | C1c | C1d | C1e | C1f
     | C20 | C21 | C22 | C23 | C24 | C25 | C26 | C27 | C28 | C29 | C2a | C2b | C2c | C2d | C2e | C2f
     | C30 | C31 | C32 | C33 | C34 | C35 | C36 | C37 | C38 | C39 | C3a | C3b | C3c | C3d | C3e | C3f
     | C40 | C41 | C42 | C43 | C44 | C45 | C46 | C47 | C48 | C49 | C4a | C4b | C4c | C4d | C4e | C4f
     | C50 | C51 | C52 | C53 | C54 | C55 | C56 | C57 | C58 | C59 | C5a | C5b | C5c | C5d | C5e | C5f
     | C60 | C61 | C62 | C63 | C64 | C65 | C66 | C67 | C68 | C69 | C6a | C6b | C6c | C6d | C6e | C6f
     | C70 | C71 | C72 | C73 | C74 | C75 | C76 | C77 | C78 | C79 | C7a | C7b | C7c | C7d | C7e | C7f
     | C80 | C81 | C82 | C83 | C84 | C85 | C86 | C87 | C88 | C89 | C8a | C8b | C8c | C8d | C8e | C8f
     | C90 | C91 | C92 | C93 | C94 | C95 | C96 | C97 | C98 | C99 | C9a | C9b | C9c | C9d | C9e | C9f
     | Ca0 | Ca1 | Ca2 | Ca3 | Ca4 | Ca5 | Ca6 | Ca7 | Ca8 | Ca9 | Caa | Cab | Cac | Cad | Cae | Caf
     | Cb0 | Cb1 | Cb2 | Cb3 | Cb4 | Cb5 | Cb6 | Cb7 | Cb8 | Cb9 | Cba | Cbb | Cbc | Cbd | Cbe | Cbf
     | Cc0 | Cc1 | Cc2 | Cc3 | Cc4 | Cc5 | Cc6 | Cc7 | Cc8 | Cc9 | Cca | Ccb | Ccc | Ccd | Cce | Ccf
     | Cd0 | Cd1 | Cd2 | Cd3 | Cd4 | Cd5 | Cd6 | Cd7 | Cd8 | Cd9 | Cda | Cdb | Cdc | Cdd | Cde | Cdf
     | Ce0 | Ce1 | Ce2 | Ce3 | Ce4 | Ce5 | Ce6 | Ce7 | Ce8 | Ce9 | Cea | Ceb | Cec | Ced | Cee | Cef
     | Cf0 | Cf1 | Cf2 | Cf3 | Cf4 | Cf5 | Cf6 | Cf7 | Cf8 | Cf9 | Cfa | Cfb | Cfc | Cfd | Cfe | Cff
  deriving (Eq, Enum, Bounded, Show, Generic, Binary)

instance Arbitrary Generic256 where                                                                                                                                                               
    arbitrary = oneof (map pure [minBound..maxBound])
