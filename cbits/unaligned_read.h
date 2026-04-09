#include <string.h>

#include "HsFFI.h"

HsWord   _hs_binary_unaligned_read_Word(HsWord8 *ptr);
HsWord16 _hs_binary_unaligned_read_Word16(HsWord8 *ptr);
HsWord32 _hs_binary_unaligned_read_Word32(HsWord8 *ptr);
HsWord64 _hs_binary_unaligned_read_Word64(HsWord8 *ptr);
HsInt    _hs_binary_unaligned_read_Int(HsWord8 *ptr);
HsInt16  _hs_binary_unaligned_read_Int16(HsWord8 *ptr);
HsInt32  _hs_binary_unaligned_read_Int32(HsWord8 *ptr);
HsInt64  _hs_binary_unaligned_read_Int64(HsWord8 *ptr);
HsFloat  _hs_binary_unaligned_read_Float(HsWord8 *ptr);
HsDouble _hs_binary_unaligned_read_Double(HsWord8 *ptr);
