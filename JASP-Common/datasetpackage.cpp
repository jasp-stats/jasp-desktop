//
// Copyright (C) 2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

#include "datasetpackage.h"

DataSetPackage::DataSetPackage()
{
	reset();
}

void DataSetPackage::reset()
{
	setDataSet(NULL);
	_archiveVersion				= Version();
	_dataArchiveVersion			= Version();
	_analysesHTML				= std::string();
	_analysesData				= Json::arrayValue;
	_warningMessage				= std::string();
	_isLoaded					= false;
	_analysesHTMLReady			= false;
	_refreshAnalysesAfterFilter = true;
	_isArchive					= false;

	setModified(false);
	resetEmptyValues();
}

void DataSetPackage::setModified(bool value)
{
	if (value != _isModified)
	{
		_isModified = value;
		isModifiedChanged(this);
	}
}


bool DataSetPackage::isColumnNameFree(std::string name) const
{
	try			{ _dataSet->columns().findIndexByName(name); return false;}
	catch(...)	{ }

	return true;
}

bool DataSetPackage::isColumnComputed(size_t colIndex) const
{
	try
	{
		const Column & normalCol = _dataSet->columns().at(colIndex);
		_computedColumns.findIndexByName(normalCol.name());
		return true;
	}
	catch(...) {}

	return false;

}

bool DataSetPackage::isColumnComputed(std::string name) const
{
	try
	{
		_computedColumns.findIndexByName(name);
		return true;
	}
	catch(...) {}

	return false;

}

bool DataSetPackage::isColumnInvalidated(size_t colIndex) const
{
	try
	{
		const Column & normalCol		= _dataSet->columns().at(colIndex);
		const ComputedColumn & compCol	= _computedColumns[normalCol.name()];

		return compCol.isInvalidated();
	}
	catch(...) {}

	return false;
}

bool DataSetPackage::isComputedColumnWithError(size_t colIndex) const
{
	try
	{
		const Column & normalCol		= _dataSet->columns().at(colIndex);
		const ComputedColumn & compCol	= _computedColumns[normalCol.name()];

		return compCol.hasError();
	}
	catch(...) {}

	return false;
}


void DataSetPackage::createComputedColumn(std::string name, Column::ColumnType columnType, size_t newColumnIndex, ComputedColumn::computedType desiredType)
{
	assert(newColumnIndex == _dataSet->columnCount() - 1);
	_computedColumns.createComputedColumn(name, columnType, &(_dataSet->columns()), desiredType);
}

ComputedColumns	* DataSetPackage::computedColumnsPointer()
{
	return &_computedColumns;
}
