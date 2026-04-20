#include <string.h>

#include "HsFFI.h"

#define UNALIGNED_READ(TYPE) Hs##TYPE _hs_binary_unaligned_read_##TYPE(HsWord8 *ptr) { Hs##TYPE result; memcpy(&result, ptr, sizeof(Hs##TYPE)); return result; }

UNALIGNED_READ(Word)
UNALIGNED_READ(Word16)
UNALIGNED_READ(Word32)
UNALIGNED_READ(Word64)
UNALIGNED_READ(Int)
UNALIGNED_READ(Int16)
UNALIGNED_READ(Int32)
UNALIGNED_READ(Int64)
UNALIGNED_READ(Float)
UNALIGNED_READ(Double)
