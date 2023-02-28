#ifndef DATASETDEFINITIONS_H
#define DATASETDEFINITIONS_H

#include "enumutilities.h"

DECLARE_ENUM_WITH_TYPE(parIdxType, unsigned char, dataRoot = 0, data, filterRoot, filter, labelRoot, label) //If this is changed then DataSetPackage::index and co must also be!

#define DEFAULT_FILTER		"# Add filters using R syntax here, see question mark for help.\n\ngeneratedFilter # by default: pass the non-R filter(s)"
#define DEFAULT_FILTER_JSON	"{\"formulas\":[]}"
#define DEFAULT_FILTER_GEN	"generatedFilter <- rep(TRUE, rowcount)"

#endif // DATASETDEFINITIONS_H
