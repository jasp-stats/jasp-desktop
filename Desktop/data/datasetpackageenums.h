#ifndef DATASETPACKAGEENUMS_H
#define DATASETPACKAGEENUMS_H

#include "enumutilities.h"

///Special roles for the different submodels of DataSetPackage. If both maxColString and columnWidthFallback are defined by a model DataSetView will only use maxColString. selected is now only used in ColumnModel, but defined here for convenience.
DECLARE_ENUM(dataPkgRoles, filter = Qt::UserRole, lines, maxColString, maxRowHeaderString, columnIsComputed, computedColumnIsInvalidated, labelsHasFilter, labelsStrList, computedColumnError,
			 value, valuesStrList, valuesDblList, columnType, computedColumnType, columnPkgIndex, columnWidthFallback, label, name, title, description, inEasyFilter, selected );

#endif // DATASETPACKAGEENUMS_H
