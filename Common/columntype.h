#ifndef COLUMNTYPE_H
#define COLUMNTYPE_H
#include "enumutilities.h"

DECLARE_ENUM(columnType,				unknown = 0, nominal = 1, nominalText = 2, ordinal = 3, scale = 4);
DECLARE_ENUM(columnTypeChangeResult,	changed, cannotConvertStringValueToInteger, cannotConvertStringValueToDouble, cannotConvertDoubleValueToInteger, unknownError);
DECLARE_ENUM(computedColumnType,		unknown, rCode, constructorCode, analysis, analysisNotComputed);
DECLARE_ENUM(dbDbl,						nan, inf, neg_inf);

#endif // COLUMNTYPE_H
