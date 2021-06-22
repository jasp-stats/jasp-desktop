//
// Copyright (C) 2015-2018 University of Amsterdam
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

#include "readstatimportdataset.h"
#include "log.h"

bool operator<(const readstat_value_t & l, const readstat_value_t & r)
{
	readstat_type_t ltype = readstat_value_type(l),
					rtype = readstat_value_type(r);

	if(ltype != rtype) return ltype < rtype;

	switch(ltype)
	{
	case READSTAT_TYPE_STRING:		return std::string(readstat_string_value(l)) < std::string(readstat_string_value(r));
	case READSTAT_TYPE_INT8:		return readstat_int8_value(l)	< readstat_int8_value(r);
	case READSTAT_TYPE_INT16:		return readstat_int16_value(l)	< readstat_int16_value(r);
	case READSTAT_TYPE_INT32:		return readstat_int32_value(l)	< readstat_int32_value(r);
	case READSTAT_TYPE_FLOAT:		return readstat_float_value(l)	< readstat_float_value(r);
	case READSTAT_TYPE_DOUBLE:		return readstat_double_value(l)	< readstat_double_value(r);
	default:						return &l < &r; //If we cannot do anything else just order them based on their pointer to make sure < is well-ordered
	}
}

ReadStatImportDataSet::~ReadStatImportDataSet()
{
	_cols.clear(); //columns themselves get delete in ImportDataSet through the vector _columns
}

void ReadStatImportDataSet::addLabelKeyValue(const std::string & labelsID, const readstat_value_t & key, const std::string & label)
{
	_labelMap[labelsID][key] = label;
}

const std::string &	ReadStatImportDataSet::getLabel(const std::string & labelsID, const readstat_value_t & key)
{
	return _labelMap[labelsID][key];
}

void ReadStatImportDataSet::addColumn(int index, ReadStatImportColumn * col)
{
	_cols[index] = col;
	ImportDataSet::addColumn(col);
}

ReadStatImportColumn * ReadStatImportDataSet::column(int index)
{
	if(_cols.count(index) > 0)	return _cols[index];
	else						return nullptr;
}

void ReadStatImportDataSet::setLabelsToColumns()
{
	for(auto & colKeyVal : _cols)
	{
		ReadStatImportColumn * col = colKeyVal.second;
		
		Log::log() << "Setting labels for column " << col->name() << std::endl;		

		if(col->hasLabels())
		{
			auto valueLabelMap = _labelMap[col->labelsID()];

			for(auto & valueLabel : valueLabelMap)
			{
						readstat_type_t			type	= readstat_value_type(valueLabel.first);
				const	readstat_value_t 	&	value	= valueLabel.first;
				const	std::string			&	label	= valueLabel.second;

				switch(type)
				{
				case READSTAT_TYPE_STRING:		col->addLabel(			readstat_string_value(value),	label);	break;
				case READSTAT_TYPE_INT8:		col->addLabel(int(		readstat_int8_value(value)),	label);	break;
				case READSTAT_TYPE_INT16:		col->addLabel(int(		readstat_int16_value(value)),	label);	break;
				case READSTAT_TYPE_INT32:		col->addLabel(int(		readstat_int32_value(value)),	label);	break;
				case READSTAT_TYPE_FLOAT:		col->addLabel(double(	readstat_float_value(value)),	label);	break;
				case READSTAT_TYPE_DOUBLE:		col->addLabel(			readstat_float_value(value),	label);	break;
				case READSTAT_TYPE_STRING_REF:																	break;
				}
			}
		}
	}
}

void ReadStatImportDataSet::setCurrentRow(int row)
{
	if(_currentRow == row) return;

	_currentRow = row;

	_progressCallback(int(float(_currentRow) / float(_expectedRows) * 100.0));
}
