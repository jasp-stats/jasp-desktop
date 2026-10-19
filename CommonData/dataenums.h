#ifndef DATASETPACKAGEENUMS_H
#define DATASETPACKAGEENUMS_H

#include "enumutilities.h"

///Special roles for the different submodels of DataSetPackage. If both maxColString and columnWidthFallback are defined by a model DataSetView will only use maxColString. selected is now only used in ColumnModel, but defined here for convenience.
DECLARE_ENUM(
	dataPkgRoles,
	name = 256, //hardcoded Qt::UserRole
	title,
	label,
	value, 
	lines, 
	filter,
	selected, 
	columnType, 
	description, 
	maxColString, 
	noSepaDisplay, // basically "display" but then without the nice separators
	shadowDisplay, 
	valueLabelPair, 
	maxRowHeaderString, 
	computedColumnError,
	computedColumnIsInvalidated, 
	nonFilteredNumericValuesCount,
	maxColumnHeaderString, 
	columnWidthFallback, 
	computedColumnType, 
	nonFilteredLevels,
	columnIsComputed, 
	labelsHasFilter, 
	columnPkgIndex, 
	valuesStrList, 
	valuesDblList, 
	inEasyFilter, 
	totalLevels,
	previewScale,
	previewOrdinal,
	previewNominal,
	id
);

DECLARE_ENUM(dataSetBaseNodeType,	unknown, dataSet, filter, column, label, workspace);

#endif // DATASETPACKAGEENUMS_H
