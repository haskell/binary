{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Self-contained WebAssembly 1.0 (MVP) binary decoder and encoder
-- built on "Data.Binary.Get" and "Data.Binary.Put".
--
-- Used as a realistic benchmark exercising LEB128 varints, @isolate@,
-- @skip@, little-endian floats, UTF-8 names, bulk @ByteString@ payloads,
-- large sum types (~170 instruction opcodes), and @Generic@ @Binary@.
module Wasm
  ( Module(..)
  , Section(..)
  , ValType(..)
  , BlockType(..)
  , FuncType(..)
  , Limits(..)
  , TableType(..)
  , GlobalType(..)
  , Import(..)
  , ImportDesc(..)
  , Export(..)
  , ExportDesc(..)
  , Global(..)
  , Elem(..)
  , FuncBody(..)
  , LocalDecl(..)
  , DataSeg(..)
  , MemArg(..)
  , Instr(..)
  , getModule
  , getModuleShallow
  , putModule
  ) where

import Control.DeepSeq (NFData)
import Control.Monad (when)
import Data.Binary (Binary)
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Int
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Data.Word
import GHC.Generics (Generic)

------------------------------------------------------------------------
-- WebAssembly 1.0 (MVP) AST

data Module = Module
  { modVersion  :: !Word32
  , modSections :: ![Section]
  } deriving (Eq, Show, Generic)

data Section
  = CustomSection !B.ByteString !B.ByteString
  | TypeSection ![FuncType]
  | ImportSection ![Import]
  | FunctionSection ![Word32]
  | TableSection ![TableType]
  | MemorySection ![Limits]
  | GlobalSection ![Global]
  | ExportSection ![Export]
  | StartSection !Word32
  | ElementSection ![Elem]
  | CodeSection ![FuncBody]
  | CodeSectionSkipped !Int
  | DataSection ![DataSeg]
  deriving (Eq, Show, Generic)

data ValType = I32 | I64 | F32 | F64
  deriving (Eq, Show, Generic)

data BlockType = BlockEmpty | BlockVal !ValType
  deriving (Eq, Show, Generic)

data FuncType = FuncType ![ValType] ![ValType]
  deriving (Eq, Show, Generic)

data Limits = Limits !Word32 !(Maybe Word32)
  deriving (Eq, Show, Generic)

data TableType = TableType !Word8 !Limits
  deriving (Eq, Show, Generic)

data GlobalType = GlobalType !ValType !Bool
  deriving (Eq, Show, Generic)

data Import = Import !B.ByteString !B.ByteString !ImportDesc
  deriving (Eq, Show, Generic)

data ImportDesc
  = ImportFunc !Word32
  | ImportTable !TableType
  | ImportMem !Limits
  | ImportGlobal !GlobalType
  deriving (Eq, Show, Generic)

data Export = Export !B.ByteString !ExportDesc
  deriving (Eq, Show, Generic)

data ExportDesc
  = ExportFunc !Word32
  | ExportTable !Word32
  | ExportMem !Word32
  | ExportGlobal !Word32
  deriving (Eq, Show, Generic)

data Global = Global !GlobalType ![Instr]
  deriving (Eq, Show, Generic)

data Elem = Elem !Word32 ![Instr] ![Word32]
  deriving (Eq, Show, Generic)

data FuncBody = FuncBody ![LocalDecl] ![Instr]
  deriving (Eq, Show, Generic)

data LocalDecl = LocalDecl !Word32 !ValType
  deriving (Eq, Show, Generic)

data DataSeg = DataSeg !Word32 ![Instr] !B.ByteString
  deriving (Eq, Show, Generic)

data MemArg = MemArg !Word32 !Word32
  deriving (Eq, Show, Generic)

data Instr
  -- Control
  = Unreachable
  | Nop
  | Block !BlockType ![Instr]
  | Loop !BlockType ![Instr]
  | If !BlockType ![Instr] ![Instr]
  | Br !Word32
  | BrIf !Word32
  | BrTable ![Word32] !Word32
  | Return
  | Call !Word32
  | CallIndirect !Word32 !Word8
  -- Parametric
  | Drop
  | Select
  -- Variable
  | LocalGet !Word32
  | LocalSet !Word32
  | LocalTee !Word32
  | GlobalGet !Word32
  | GlobalSet !Word32
  -- Memory
  | I32Load !MemArg
  | I64Load !MemArg
  | F32Load !MemArg
  | F64Load !MemArg
  | I32Load8S !MemArg
  | I32Load8U !MemArg
  | I32Load16S !MemArg
  | I32Load16U !MemArg
  | I64Load8S !MemArg
  | I64Load8U !MemArg
  | I64Load16S !MemArg
  | I64Load16U !MemArg
  | I64Load32S !MemArg
  | I64Load32U !MemArg
  | I32Store !MemArg
  | I64Store !MemArg
  | F32Store !MemArg
  | F64Store !MemArg
  | I32Store8 !MemArg
  | I32Store16 !MemArg
  | I64Store8 !MemArg
  | I64Store16 !MemArg
  | I64Store32 !MemArg
  | MemorySize !Word8
  | MemoryGrow !Word8
  -- Constants
  | I32Const !Int32
  | I64Const !Int64
  | F32Const !Float
  | F64Const !Double
  -- i32 comparisons
  | I32Eqz | I32Eq | I32Ne | I32LtS | I32LtU | I32GtS | I32GtU | I32LeS | I32LeU | I32GeS | I32GeU
  -- i64 comparisons
  | I64Eqz | I64Eq | I64Ne | I64LtS | I64LtU | I64GtS | I64GtU | I64LeS | I64LeU | I64GeS | I64GeU
  -- f32 comparisons
  | F32Eq | F32Ne | F32Lt | F32Gt | F32Le | F32Ge
  -- f64 comparisons
  | F64Eq | F64Ne | F64Lt | F64Gt | F64Le | F64Ge
  -- i32 arithmetic
  | I32Clz | I32Ctz | I32Popcnt | I32Add | I32Sub | I32Mul | I32DivS | I32DivU | I32RemS | I32RemU
  | I32And | I32Or | I32Xor | I32Shl | I32ShrS | I32ShrU | I32Rotl | I32Rotr
  -- i64 arithmetic
  | I64Clz | I64Ctz | I64Popcnt | I64Add | I64Sub | I64Mul | I64DivS | I64DivU | I64RemS | I64RemU
  | I64And | I64Or | I64Xor | I64Shl | I64ShrS | I64ShrU | I64Rotl | I64Rotr
  -- f32 arithmetic
  | F32Abs | F32Neg | F32Ceil | F32Floor | F32Trunc | F32Nearest | F32Sqrt
  | F32Add | F32Sub | F32Mul | F32Div | F32Min | F32Max | F32Copysign
  -- f64 arithmetic
  | F64Abs | F64Neg | F64Ceil | F64Floor | F64Trunc | F64Nearest | F64Sqrt
  | F64Add | F64Sub | F64Mul | F64Div | F64Min | F64Max | F64Copysign
  -- Conversions
  | I32WrapI64
  | I32TruncF32S | I32TruncF32U | I32TruncF64S | I32TruncF64U
  | I64ExtendI32S | I64ExtendI32U
  | I64TruncF32S | I64TruncF32U | I64TruncF64S | I64TruncF64U
  | F32ConvertI32S | F32ConvertI32U | F32ConvertI64S | F32ConvertI64U | F32DemoteF64
  | F64ConvertI32S | F64ConvertI32U | F64ConvertI64S | F64ConvertI64U | F64PromoteF32
  | I32ReinterpretF32 | I64ReinterpretF64 | F32ReinterpretI32 | F64ReinterpretI64
  deriving (Eq, Show, Generic)

instance NFData Module
instance NFData Section
instance NFData ValType
instance NFData BlockType
instance NFData FuncType
instance NFData Limits
instance NFData TableType
instance NFData GlobalType
instance NFData Import
instance NFData ImportDesc
instance NFData Export
instance NFData ExportDesc
instance NFData Global
instance NFData Elem
instance NFData FuncBody
instance NFData LocalDecl
instance NFData DataSeg
instance NFData MemArg
instance NFData Instr

instance Binary Module
instance Binary Section
instance Binary ValType
instance Binary BlockType
instance Binary FuncType
instance Binary Limits
instance Binary TableType
instance Binary GlobalType
instance Binary Import
instance Binary ImportDesc
instance Binary Export
instance Binary ExportDesc
instance Binary Global
instance Binary Elem
instance Binary FuncBody
instance Binary LocalDecl
instance Binary DataSeg
instance Binary MemArg
instance Binary Instr

------------------------------------------------------------------------
-- LEB128 and Vector Primitives

getULEB128 :: Get Word32
getULEB128 = go 0 0
  where
    go :: Word32 -> Int -> Get Word32
    go !acc !sh = do
      b <- getWord8
      let !acc' = acc .|. (fromIntegral (b .&. 0x7f) `shiftL` sh)
      if b .&. 0x80 == 0
        then return acc'
        else if sh >= 28
          then fail "ULEB128 overflow"
          else go acc' (sh + 7)
{-# INLINE getULEB128 #-}

getSLEB128_32 :: Get Int32
getSLEB128_32 = go 0 0
  where
    go :: Int32 -> Int -> Get Int32
    go !acc !sh = do
      b <- getWord8
      let !acc' = acc .|. (fromIntegral (b .&. 0x7f) `shiftL` sh)
          !sh' = sh + 7
      if b .&. 0x80 == 0
        then if sh' < 32 && (b .&. 0x40 /= 0)
               then return (acc' .|. ((-1) `shiftL` sh'))
               else return acc'
        else if sh >= 28
          then fail "SLEB128_32 overflow"
          else go acc' sh'
{-# INLINE getSLEB128_32 #-}

getSLEB128_64 :: Get Int64
getSLEB128_64 = go 0 0
  where
    go :: Int64 -> Int -> Get Int64
    go !acc !sh = do
      b <- getWord8
      let !acc' = acc .|. (fromIntegral (b .&. 0x7f) `shiftL` sh)
          !sh' = sh + 7
      if b .&. 0x80 == 0
        then if sh' < 64 && (b .&. 0x40 /= 0)
               then return (acc' .|. ((-1) `shiftL` sh'))
               else return acc'
        else if sh >= 63
          then fail "SLEB128_64 overflow"
          else go acc' sh'
{-# INLINE getSLEB128_64 #-}

putULEB128 :: Word32 -> Put
putULEB128 = go
  where
    go !n
      | n < 0x80  = putWord8 (fromIntegral n)
      | otherwise = putWord8 (fromIntegral (n .&. 0x7f) .|. 0x80) <> go (n `shiftR` 7)
{-# INLINE putULEB128 #-}

putSLEB128_32 :: Int32 -> Put
putSLEB128_32 = go
  where
    go !n =
      let !b = fromIntegral (n .&. 0x7f) :: Word8
          !n' = n `shiftR` 7
          !done = (n' == 0 && b .&. 0x40 == 0) || (n' == -1 && b .&. 0x40 /= 0)
      in if done
           then putWord8 b
           else putWord8 (b .|. 0x80) <> go n'
{-# INLINE putSLEB128_32 #-}

putSLEB128_64 :: Int64 -> Put
putSLEB128_64 = go
  where
    go !n =
      let !b = fromIntegral (n .&. 0x7f) :: Word8
          !n' = n `shiftR` 7
          !done = (n' == 0 && b .&. 0x40 == 0) || (n' == -1 && b .&. 0x40 /= 0)
      in if done
           then putWord8 b
           else putWord8 (b .|. 0x80) <> go n'
{-# INLINE putSLEB128_64 #-}

-- | Strict accumulator-based counted repetition (analogous to attoparsec's @count@).
getVec :: Get a -> Get [a]
getVec item = do
  n <- getULEB128
  go [] n
  where
    go acc 0 = return $! reverse acc
    go acc !i = do
      !x <- item
      go (x : acc) (i - 1)
{-# INLINE getVec #-}

putVec :: (a -> Put) -> [a] -> Put
putVec f xs = putULEB128 (fromIntegral (length xs)) <> foldMap f xs
{-# INLINE putVec #-}

getName :: Get B.ByteString
getName = do
  n <- fromIntegral <$> getULEB128
  getByteString n
{-# INLINE getName #-}

putName :: B.ByteString -> Put
putName bs = putULEB128 (fromIntegral (B.length bs)) <> putByteString bs
{-# INLINE putName #-}

------------------------------------------------------------------------
-- Decoder

getModule :: Get Module
getModule = getModuleWith False

getModuleShallow :: Get Module
getModuleShallow = getModuleWith True

getModuleWith :: Bool -> Get Module
getModuleWith skipCode = do
  magic <- getByteString 4
  when (magic /= "\0asm") $ fail "Invalid WebAssembly magic header"
  ver <- getWord32le
  when (ver /= 1) $ fail ("Unsupported WebAssembly version: " ++ show ver)
  secs <- getSections []
  return (Module ver secs)
  where
    getSections acc = do
      done <- isEmpty
      if done
        then return $! reverse acc
        else do
          !sec <- getSection skipCode
          getSections (sec : acc)

getSection :: Bool -> Get Section
getSection skipCode = do
  secId <- getWord8
  secSize <- fromIntegral <$> getULEB128
  case secId of
    0  -> isolate secSize $ do
            name <- getName
            pos <- bytesRead
            payload <- getByteString (secSize - fromIntegral pos)
            return (CustomSection name payload)
    1  -> isolate secSize (TypeSection <$> getVec getFuncType)
    2  -> isolate secSize (ImportSection <$> getVec getImport)
    3  -> isolate secSize (FunctionSection <$> getVec getULEB128)
    4  -> isolate secSize (TableSection <$> getVec getTableType)
    5  -> isolate secSize (MemorySection <$> getVec getLimits)
    6  -> isolate secSize (GlobalSection <$> getVec getGlobal)
    7  -> isolate secSize (ExportSection <$> getVec getExport)
    8  -> isolate secSize (StartSection <$> getULEB128)
    9  -> isolate secSize (ElementSection <$> getVec getElem)
    10 -> if skipCode
            then skip secSize >> return (CodeSectionSkipped secSize)
            else isolate secSize (CodeSection <$> getVec getFuncBody)
    11 -> isolate secSize (DataSection <$> getVec getDataSeg)
    _  -> fail ("Unknown WebAssembly section id: " ++ show secId)

getValType :: Get ValType
getValType = do
  b <- getWord8
  case b of
    0x7f -> return I32
    0x7e -> return I64
    0x7d -> return F32
    0x7c -> return F64
    _    -> fail ("Unknown ValType: " ++ show b)
{-# INLINE getValType #-}

getBlockType :: Get BlockType
getBlockType = do
  b <- getWord8
  case b of
    0x40 -> return BlockEmpty
    0x7f -> return (BlockVal I32)
    0x7e -> return (BlockVal I64)
    0x7d -> return (BlockVal F32)
    0x7c -> return (BlockVal F64)
    _    -> fail ("Unknown BlockType: " ++ show b)
{-# INLINE getBlockType #-}

getFuncType :: Get FuncType
getFuncType = do
  tag <- getWord8
  when (tag /= 0x60) $ fail ("Expected functype tag 0x60, got " ++ show tag)
  FuncType <$> getVec getValType <*> getVec getValType

getLimits :: Get Limits
getLimits = do
  flag <- getWord8
  case flag of
    0x00 -> Limits <$> getULEB128 <*> pure Nothing
    0x01 -> Limits <$> getULEB128 <*> (Just <$> getULEB128)
    _    -> fail ("Unknown limits flag: " ++ show flag)

getTableType :: Get TableType
getTableType = TableType <$> getWord8 <*> getLimits

getGlobalType :: Get GlobalType
getGlobalType = do
  vt <- getValType
  mut <- getWord8
  case mut of
    0 -> return (GlobalType vt False)
    1 -> return (GlobalType vt True)
    _ -> fail ("Unknown global mutability: " ++ show mut)

getImport :: Get Import
getImport = do
  m <- getName
  n <- getName
  tag <- getWord8
  desc <- case tag of
    0x00 -> ImportFunc <$> getULEB128
    0x01 -> ImportTable <$> getTableType
    0x02 -> ImportMem <$> getLimits
    0x03 -> ImportGlobal <$> getGlobalType
    _    -> fail ("Unknown import desc: " ++ show tag)
  return (Import m n desc)

getExport :: Get Export
getExport = do
  n <- getName
  tag <- getWord8
  desc <- case tag of
    0x00 -> ExportFunc <$> getULEB128
    0x01 -> ExportTable <$> getULEB128
    0x02 -> ExportMem <$> getULEB128
    0x03 -> ExportGlobal <$> getULEB128
    _    -> fail ("Unknown export desc: " ++ show tag)
  return (Export n desc)

getGlobal :: Get Global
getGlobal = Global <$> getGlobalType <*> getExpr

getElem :: Get Elem
getElem = Elem <$> getULEB128 <*> getExpr <*> getVec getULEB128

getFuncBody :: Get FuncBody
getFuncBody = do
  bodySize <- fromIntegral <$> getULEB128
  isolate bodySize $ do
    locals <- getVec (LocalDecl <$> getULEB128 <*> getValType)
    body <- getExpr
    return (FuncBody locals body)

getDataSeg :: Get DataSeg
getDataSeg = DataSeg <$> getULEB128 <*> getExpr <*> getName

getMemArg :: Get MemArg
getMemArg = MemArg <$> getULEB128 <*> getULEB128
{-# INLINE getMemArg #-}

getExpr :: Get [Instr]
getExpr = go []
  where
    go acc = do
      op <- getWord8
      if op == 0x0b
        then return $! reverse acc
        else do
          !instr <- getInstr op
          go (instr : acc)

getIfBranches :: Get ([Instr], [Instr])
getIfBranches = goThen []
  where
    goThen acc = do
      op <- getWord8
      case op of
        0x0b -> return (reverse acc, [])
        0x05 -> do
          elseBranch <- getExpr
          return (reverse acc, elseBranch)
        _ -> do
          !instr <- getInstr op
          goThen (instr : acc)

getInstr :: Word8 -> Get Instr
getInstr op = case op of
  0x00 -> return Unreachable
  0x01 -> return Nop
  0x02 -> Block <$> getBlockType <*> getExpr
  0x03 -> Loop <$> getBlockType <*> getExpr
  0x04 -> do
    bt <- getBlockType
    (t, e) <- getIfBranches
    return (If bt t e)
  0x0c -> Br <$> getULEB128
  0x0d -> BrIf <$> getULEB128
  0x0e -> BrTable <$> getVec getULEB128 <*> getULEB128
  0x0f -> return Return
  0x10 -> Call <$> getULEB128
  0x11 -> CallIndirect <$> getULEB128 <*> getWord8
  0x1a -> return Drop
  0x1b -> return Select
  0x20 -> LocalGet <$> getULEB128
  0x21 -> LocalSet <$> getULEB128
  0x22 -> LocalTee <$> getULEB128
  0x23 -> GlobalGet <$> getULEB128
  0x24 -> GlobalSet <$> getULEB128
  0x28 -> I32Load <$> getMemArg
  0x29 -> I64Load <$> getMemArg
  0x2a -> F32Load <$> getMemArg
  0x2b -> F64Load <$> getMemArg
  0x2c -> I32Load8S <$> getMemArg
  0x2d -> I32Load8U <$> getMemArg
  0x2e -> I32Load16S <$> getMemArg
  0x2f -> I32Load16U <$> getMemArg
  0x30 -> I64Load8S <$> getMemArg
  0x31 -> I64Load8U <$> getMemArg
  0x32 -> I64Load16S <$> getMemArg
  0x33 -> I64Load16U <$> getMemArg
  0x34 -> I64Load32S <$> getMemArg
  0x35 -> I64Load32U <$> getMemArg
  0x36 -> I32Store <$> getMemArg
  0x37 -> I64Store <$> getMemArg
  0x38 -> F32Store <$> getMemArg
  0x39 -> F64Store <$> getMemArg
  0x3a -> I32Store8 <$> getMemArg
  0x3b -> I32Store16 <$> getMemArg
  0x3c -> I64Store8 <$> getMemArg
  0x3d -> I64Store16 <$> getMemArg
  0x3e -> I64Store32 <$> getMemArg
  0x3f -> MemorySize <$> getWord8
  0x40 -> MemoryGrow <$> getWord8
  0x41 -> I32Const <$> getSLEB128_32
  0x42 -> I64Const <$> getSLEB128_64
  0x43 -> F32Const <$> getFloatle
  0x44 -> F64Const <$> getDoublele
  0x45 -> return I32Eqz
  0x46 -> return I32Eq
  0x47 -> return I32Ne
  0x48 -> return I32LtS
  0x49 -> return I32LtU
  0x4a -> return I32GtS
  0x4b -> return I32GtU
  0x4c -> return I32LeS
  0x4d -> return I32LeU
  0x4e -> return I32GeS
  0x4f -> return I32GeU
  0x50 -> return I64Eqz
  0x51 -> return I64Eq
  0x52 -> return I64Ne
  0x53 -> return I64LtS
  0x54 -> return I64LtU
  0x55 -> return I64GtS
  0x56 -> return I64GtU
  0x57 -> return I64LeS
  0x58 -> return I64LeU
  0x59 -> return I64GeS
  0x5a -> return I64GeU
  0x5b -> return F32Eq
  0x5c -> return F32Ne
  0x5d -> return F32Lt
  0x5e -> return F32Gt
  0x5f -> return F32Le
  0x60 -> return F32Ge
  0x61 -> return F64Eq
  0x62 -> return F64Ne
  0x63 -> return F64Lt
  0x64 -> return F64Gt
  0x65 -> return F64Le
  0x66 -> return F64Ge
  0x67 -> return I32Clz
  0x68 -> return I32Ctz
  0x69 -> return I32Popcnt
  0x6a -> return I32Add
  0x6b -> return I32Sub
  0x6c -> return I32Mul
  0x6d -> return I32DivS
  0x6e -> return I32DivU
  0x6f -> return I32RemS
  0x70 -> return I32RemU
  0x71 -> return I32And
  0x72 -> return I32Or
  0x73 -> return I32Xor
  0x74 -> return I32Shl
  0x75 -> return I32ShrS
  0x76 -> return I32ShrU
  0x77 -> return I32Rotl
  0x78 -> return I32Rotr
  0x79 -> return I64Clz
  0x7a -> return I64Ctz
  0x7b -> return I64Popcnt
  0x7c -> return I64Add
  0x7d -> return I64Sub
  0x7e -> return I64Mul
  0x7f -> return I64DivS
  0x80 -> return I64DivU
  0x81 -> return I64RemS
  0x82 -> return I64RemU
  0x83 -> return I64And
  0x84 -> return I64Or
  0x85 -> return I64Xor
  0x86 -> return I64Shl
  0x87 -> return I64ShrS
  0x88 -> return I64ShrU
  0x89 -> return I64Rotl
  0x8a -> return I64Rotr
  0x8b -> return F32Abs
  0x8c -> return F32Neg
  0x8d -> return F32Ceil
  0x8e -> return F32Floor
  0x8f -> return F32Trunc
  0x90 -> return F32Nearest
  0x91 -> return F32Sqrt
  0x92 -> return F32Add
  0x93 -> return F32Sub
  0x94 -> return F32Mul
  0x95 -> return F32Div
  0x96 -> return F32Min
  0x97 -> return F32Max
  0x98 -> return F32Copysign
  0x99 -> return F64Abs
  0x9a -> return F64Neg
  0x9b -> return F64Ceil
  0x9c -> return F64Floor
  0x9d -> return F64Trunc
  0x9e -> return F64Nearest
  0x9f -> return F64Sqrt
  0xa0 -> return F64Add
  0xa1 -> return F64Sub
  0xa2 -> return F64Mul
  0xa3 -> return F64Div
  0xa4 -> return F64Min
  0xa5 -> return F64Max
  0xa6 -> return F64Copysign
  0xa7 -> return I32WrapI64
  0xa8 -> return I32TruncF32S
  0xa9 -> return I32TruncF32U
  0xaa -> return I32TruncF64S
  0xab -> return I32TruncF64U
  0xac -> return I64ExtendI32S
  0xad -> return I64ExtendI32U
  0xae -> return I64TruncF32S
  0xaf -> return I64TruncF32U
  0xb0 -> return I64TruncF64S
  0xb1 -> return I64TruncF64U
  0xb2 -> return F32ConvertI32S
  0xb3 -> return F32ConvertI32U
  0xb4 -> return F32ConvertI64S
  0xb5 -> return F32ConvertI64U
  0xb6 -> return F32DemoteF64
  0xb7 -> return F64ConvertI32S
  0xb8 -> return F64ConvertI32U
  0xb9 -> return F64ConvertI64S
  0xba -> return F64ConvertI64U
  0xbb -> return F64PromoteF32
  0xbc -> return I32ReinterpretF32
  0xbd -> return I64ReinterpretF64
  0xbe -> return F32ReinterpretI32
  0xbf -> return F64ReinterpretI64
  _    -> fail ("Unknown WebAssembly opcode: " ++ show op)

------------------------------------------------------------------------
-- Encoder

putModule :: Module -> Put
putModule (Module ver secs) =
  putByteString "\0asm" <> putWord32le ver <> foldMap putSection secs

putSection :: Section -> Put
putSection sec = case sec of
  CustomSection name payload -> putSizedSection 0 (putName name <> putByteString payload)
  TypeSection ts             -> putSizedSection 1 (putVec putFuncType ts)
  ImportSection imps         -> putSizedSection 2 (putVec putImport imps)
  FunctionSection fs         -> putSizedSection 3 (putVec putULEB128 fs)
  TableSection ts            -> putSizedSection 4 (putVec putTableType ts)
  MemorySection ms           -> putSizedSection 5 (putVec putLimits ms)
  GlobalSection gs           -> putSizedSection 6 (putVec putGlobal gs)
  ExportSection es           -> putSizedSection 7 (putVec putExport es)
  StartSection idx           -> putSizedSection 8 (putULEB128 idx)
  ElementSection els         -> putSizedSection 9 (putVec putElem els)
  CodeSection bodies         -> putSizedSection 10 (putVec putFuncBody bodies)
  CodeSectionSkipped _       -> error "putSection: cannot encode CodeSectionSkipped"
  DataSection segs           -> putSizedSection 11 (putVec putDataSeg segs)

putSizedSection :: Word8 -> Put -> Put
putSizedSection secId body =
  let lbs = runPut body
  in putWord8 secId <> putULEB128 (fromIntegral (L.length lbs)) <> putLazyByteString lbs

putValType :: ValType -> Put
putValType I32 = putWord8 0x7f
putValType I64 = putWord8 0x7e
putValType F32 = putWord8 0x7d
putValType F64 = putWord8 0x7c

putBlockType :: BlockType -> Put
putBlockType BlockEmpty    = putWord8 0x40
putBlockType (BlockVal vt) = putValType vt

putFuncType :: FuncType -> Put
putFuncType (FuncType params results) =
  putWord8 0x60 <> putVec putValType params <> putVec putValType results

putLimits :: Limits -> Put
putLimits (Limits mn Nothing)   = putWord8 0x00 <> putULEB128 mn
putLimits (Limits mn (Just mx)) = putWord8 0x01 <> putULEB128 mn <> putULEB128 mx

putTableType :: TableType -> Put
putTableType (TableType et lim) = putWord8 et <> putLimits lim

putGlobalType :: GlobalType -> Put
putGlobalType (GlobalType vt mut) = putValType vt <> putWord8 (if mut then 1 else 0)

putImport :: Import -> Put
putImport (Import m n desc) =
  putName m <> putName n <> case desc of
    ImportFunc idx  -> putWord8 0x00 <> putULEB128 idx
    ImportTable tt  -> putWord8 0x01 <> putTableType tt
    ImportMem lim   -> putWord8 0x02 <> putLimits lim
    ImportGlobal gt -> putWord8 0x03 <> putGlobalType gt

putExport :: Export -> Put
putExport (Export n desc) =
  putName n <> case desc of
    ExportFunc idx   -> putWord8 0x00 <> putULEB128 idx
    ExportTable idx  -> putWord8 0x01 <> putULEB128 idx
    ExportMem idx    -> putWord8 0x02 <> putULEB128 idx
    ExportGlobal idx -> putWord8 0x03 <> putULEB128 idx

putGlobal :: Global -> Put
putGlobal (Global gt expr) = putGlobalType gt <> putExpr expr

putElem :: Elem -> Put
putElem (Elem idx offset funcs) =
  putULEB128 idx <> putExpr offset <> putVec putULEB128 funcs

putFuncBody :: FuncBody -> Put
putFuncBody (FuncBody locals expr) =
  let lbs = runPut (putVec putLocalDecl locals <> putExpr expr)
  in putULEB128 (fromIntegral (L.length lbs)) <> putLazyByteString lbs

putLocalDecl :: LocalDecl -> Put
putLocalDecl (LocalDecl cnt vt) = putULEB128 cnt <> putValType vt

putDataSeg :: DataSeg -> Put
putDataSeg (DataSeg idx offset bytes) =
  putULEB128 idx <> putExpr offset <> putName bytes

putMemArg :: MemArg -> Put
putMemArg (MemArg align offset) = putULEB128 align <> putULEB128 offset

putExpr :: [Instr] -> Put
putExpr instrs = foldMap putInstr instrs <> putWord8 0x0b

putInstr :: Instr -> Put
putInstr instr = case instr of
  Unreachable          -> putWord8 0x00
  Nop                  -> putWord8 0x01
  Block bt body        -> putWord8 0x02 <> putBlockType bt <> putExpr body
  Loop bt body         -> putWord8 0x03 <> putBlockType bt <> putExpr body
  If bt t []           -> putWord8 0x04 <> putBlockType bt <> putExpr t
  If bt t e            -> putWord8 0x04 <> putBlockType bt <> foldMap putInstr t <> putWord8 0x05 <> putExpr e
  Br l                 -> putWord8 0x0c <> putULEB128 l
  BrIf l               -> putWord8 0x0d <> putULEB128 l
  BrTable ls d         -> putWord8 0x0e <> putVec putULEB128 ls <> putULEB128 d
  Return               -> putWord8 0x0f
  Call idx             -> putWord8 0x10 <> putULEB128 idx
  CallIndirect idx tbl -> putWord8 0x11 <> putULEB128 idx <> putWord8 tbl
  Drop                 -> putWord8 0x1a
  Select               -> putWord8 0x1b
  LocalGet idx         -> putWord8 0x20 <> putULEB128 idx
  LocalSet idx         -> putWord8 0x21 <> putULEB128 idx
  LocalTee idx         -> putWord8 0x22 <> putULEB128 idx
  GlobalGet idx        -> putWord8 0x23 <> putULEB128 idx
  GlobalSet idx        -> putWord8 0x24 <> putULEB128 idx
  I32Load ma           -> putWord8 0x28 <> putMemArg ma
  I64Load ma           -> putWord8 0x29 <> putMemArg ma
  F32Load ma           -> putWord8 0x2a <> putMemArg ma
  F64Load ma           -> putWord8 0x2b <> putMemArg ma
  I32Load8S ma         -> putWord8 0x2c <> putMemArg ma
  I32Load8U ma         -> putWord8 0x2d <> putMemArg ma
  I32Load16S ma        -> putWord8 0x2e <> putMemArg ma
  I32Load16U ma        -> putWord8 0x2f <> putMemArg ma
  I64Load8S ma         -> putWord8 0x30 <> putMemArg ma
  I64Load8U ma         -> putWord8 0x31 <> putMemArg ma
  I64Load16S ma        -> putWord8 0x32 <> putMemArg ma
  I64Load16U ma        -> putWord8 0x33 <> putMemArg ma
  I64Load32S ma        -> putWord8 0x34 <> putMemArg ma
  I64Load32U ma        -> putWord8 0x35 <> putMemArg ma
  I32Store ma          -> putWord8 0x36 <> putMemArg ma
  I64Store ma          -> putWord8 0x37 <> putMemArg ma
  F32Store ma          -> putWord8 0x38 <> putMemArg ma
  F64Store ma          -> putWord8 0x39 <> putMemArg ma
  I32Store8 ma         -> putWord8 0x3a <> putMemArg ma
  I32Store16 ma        -> putWord8 0x3b <> putMemArg ma
  I64Store8 ma         -> putWord8 0x3c <> putMemArg ma
  I64Store16 ma        -> putWord8 0x3d <> putMemArg ma
  I64Store32 ma        -> putWord8 0x3e <> putMemArg ma
  MemorySize m         -> putWord8 0x3f <> putWord8 m
  MemoryGrow m         -> putWord8 0x40 <> putWord8 m
  I32Const n           -> putWord8 0x41 <> putSLEB128_32 n
  I64Const n           -> putWord8 0x42 <> putSLEB128_64 n
  F32Const x           -> putWord8 0x43 <> putFloatle x
  F64Const x           -> putWord8 0x44 <> putDoublele x
  I32Eqz               -> putWord8 0x45
  I32Eq                -> putWord8 0x46
  I32Ne                -> putWord8 0x47
  I32LtS               -> putWord8 0x48
  I32LtU               -> putWord8 0x49
  I32GtS               -> putWord8 0x4a
  I32GtU               -> putWord8 0x4b
  I32LeS               -> putWord8 0x4c
  I32LeU               -> putWord8 0x4d
  I32GeS               -> putWord8 0x4e
  I32GeU               -> putWord8 0x4f
  I64Eqz               -> putWord8 0x50
  I64Eq                -> putWord8 0x51
  I64Ne                -> putWord8 0x52
  I64LtS               -> putWord8 0x53
  I64LtU               -> putWord8 0x54
  I64GtS               -> putWord8 0x55
  I64GtU               -> putWord8 0x56
  I64LeS               -> putWord8 0x57
  I64LeU               -> putWord8 0x58
  I64GeS               -> putWord8 0x59
  I64GeU               -> putWord8 0x5a
  F32Eq                -> putWord8 0x5b
  F32Ne                -> putWord8 0x5c
  F32Lt                -> putWord8 0x5d
  F32Gt                -> putWord8 0x5e
  F32Le                -> putWord8 0x5f
  F32Ge                -> putWord8 0x60
  F64Eq                -> putWord8 0x61
  F64Ne                -> putWord8 0x62
  F64Lt                -> putWord8 0x63
  F64Gt                -> putWord8 0x64
  F64Le                -> putWord8 0x65
  F64Ge                -> putWord8 0x66
  I32Clz               -> putWord8 0x67
  I32Ctz               -> putWord8 0x68
  I32Popcnt            -> putWord8 0x69
  I32Add               -> putWord8 0x6a
  I32Sub               -> putWord8 0x6b
  I32Mul               -> putWord8 0x6c
  I32DivS              -> putWord8 0x6d
  I32DivU              -> putWord8 0x6e
  I32RemS              -> putWord8 0x6f
  I32RemU              -> putWord8 0x70
  I32And               -> putWord8 0x71
  I32Or                -> putWord8 0x72
  I32Xor               -> putWord8 0x73
  I32Shl               -> putWord8 0x74
  I32ShrS              -> putWord8 0x75
  I32ShrU              -> putWord8 0x76
  I32Rotl              -> putWord8 0x77
  I32Rotr              -> putWord8 0x78
  I64Clz               -> putWord8 0x79
  I64Ctz               -> putWord8 0x7a
  I64Popcnt            -> putWord8 0x7b
  I64Add               -> putWord8 0x7c
  I64Sub               -> putWord8 0x7d
  I64Mul               -> putWord8 0x7e
  I64DivS              -> putWord8 0x7f
  I64DivU              -> putWord8 0x80
  I64RemS              -> putWord8 0x81
  I64RemU              -> putWord8 0x82
  I64And               -> putWord8 0x83
  I64Or                -> putWord8 0x84
  I64Xor               -> putWord8 0x85
  I64Shl               -> putWord8 0x86
  I64ShrS              -> putWord8 0x87
  I64ShrU              -> putWord8 0x88
  I64Rotl              -> putWord8 0x89
  I64Rotr              -> putWord8 0x8a
  F32Abs               -> putWord8 0x8b
  F32Neg               -> putWord8 0x8c
  F32Ceil              -> putWord8 0x8d
  F32Floor             -> putWord8 0x8e
  F32Trunc             -> putWord8 0x8f
  F32Nearest           -> putWord8 0x90
  F32Sqrt              -> putWord8 0x91
  F32Add               -> putWord8 0x92
  F32Sub               -> putWord8 0x93
  F32Mul               -> putWord8 0x94
  F32Div               -> putWord8 0x95
  F32Min               -> putWord8 0x96
  F32Max               -> putWord8 0x97
  F32Copysign          -> putWord8 0x98
  F64Abs               -> putWord8 0x99
  F64Neg               -> putWord8 0x9a
  F64Ceil              -> putWord8 0x9b
  F64Floor             -> putWord8 0x9c
  F64Trunc             -> putWord8 0x9d
  F64Nearest           -> putWord8 0x9e
  F64Sqrt              -> putWord8 0x9f
  F64Add               -> putWord8 0xa0
  F64Sub               -> putWord8 0xa1
  F64Mul               -> putWord8 0xa2
  F64Div               -> putWord8 0xa3
  F64Min               -> putWord8 0xa4
  F64Max               -> putWord8 0xa5
  F64Copysign          -> putWord8 0xa6
  I32WrapI64           -> putWord8 0xa7
  I32TruncF32S         -> putWord8 0xa8
  I32TruncF32U         -> putWord8 0xa9
  I32TruncF64S         -> putWord8 0xaa
  I32TruncF64U         -> putWord8 0xab
  I64ExtendI32S        -> putWord8 0xac
  I64ExtendI32U        -> putWord8 0xad
  I64TruncF32S         -> putWord8 0xae
  I64TruncF32U         -> putWord8 0xaf
  I64TruncF64S         -> putWord8 0xb0
  I64TruncF64U         -> putWord8 0xb1
  F32ConvertI32S       -> putWord8 0xb2
  F32ConvertI32U       -> putWord8 0xb3
  F32ConvertI64S       -> putWord8 0xb4
  F32ConvertI64U       -> putWord8 0xb5
  F32DemoteF64         -> putWord8 0xb6
  F64ConvertI32S       -> putWord8 0xb7
  F64ConvertI32U       -> putWord8 0xb8
  F64ConvertI64S       -> putWord8 0xb9
  F64ConvertI64U       -> putWord8 0xba
  F64PromoteF32        -> putWord8 0xbb
  I32ReinterpretF32    -> putWord8 0xbc
  I64ReinterpretF64    -> putWord8 0xbd
  F32ReinterpretI32    -> putWord8 0xbe
  F64ReinterpretI64    -> putWord8 0xbf
