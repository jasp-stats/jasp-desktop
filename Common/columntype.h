#ifndef COLUMNTYPE_H
#define COLUMNTYPE_H
#include "enumutilities.h"

DECLARE_ENUM(columnType,				unknown = 0, nominal = 1, nominalText = 2, ordinal = 4, scale = 8);
DECLARE_ENUM(columnTypeChangeResult,	changed, cannotConvertStringValueToInteger, cannotConvertStringValueToDouble, cannotConvertDoubleValueToInteger, unknownError);

#endif // COLUMNTYPE_H
