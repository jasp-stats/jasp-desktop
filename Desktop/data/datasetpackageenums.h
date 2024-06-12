#ifndef DATASETPACKAGEENUMS_H
#define DATASETPACKAGEENUMS_H

#include "enumutilities.h"

///Special roles for the different submodels of DataSetPackage. If both maxColString and columnWidthFallback are defined by a model DataSetView will only use maxColString. selected is now only used in ColumnModel, but defined here for convenience.
DECLARE_ENUM(
	dataPkgRoles, 
		filter				= Qt::UserRole, 
		name, 
		title, 
		label,
		value, 
		lines, 
		selected, 
		columnType, 
		description, 
		maxColString, 
		shadowDisplay, 
		valueLabelPair, 
		maxRowHeaderString, 
		computedColumnError,
		computedColumnIsInvalidated, 
		maxColumnHeaderString, 
		columnWidthFallback, 
		computedColumnType, 
		totalNumericValues,
		totalLevels,
		columnIsComputed, 
		labelsHasFilter, 
		columnPkgIndex, 
		labelsStrList, 
		valuesStrList, 
		valuesDblList, 
		inEasyFilter, 
		previewScale,
		previewOrdinal,
		previewNominal
);

#endif // DATASETPACKAGEENUMS_H
