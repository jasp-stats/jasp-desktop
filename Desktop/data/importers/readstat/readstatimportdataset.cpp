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
#include "utils.h"
#include "columnutils.h"

bool operator<(const readstat_value_t & l, const readstat_value_t & r)
{
	try 
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
	catch(...)
	{
		Log::log() << "bool operator<(const readstat_value_t & l, const readstat_value_t & r) had some kind of problem..." << std::endl;
		return &l < &r; //Try to keep things in order
	}
}

ReadStatImportDataSet::~ReadStatImportDataSet()
{
	_cols.clear(); //columns themselves get delete in ImportDataSet through the vector _columns
}

void ReadStatImportDataSet::addLabelKeyValue(const std::string & labelsID, const readstat_value_t & key, const std::string & label)
{
	//Log::log()		<< "addLabelKeyValue(labelsID='" << labelsID << "', key='" << std::flush;
	
	const std::string str = ReadStatImportColumn::readstatValueToString(key);
	
	//Log::log(false) << str << "', label='" << label << "')" << std::endl;

	_labelMap[labelsID][str] = std::make_pair(readstat_value_type(key), label);
}

const std::string &	ReadStatImportDataSet::getLabel(const std::string & labelsID, const readstat_value_t & key)
{
	return _labelMap[labelsID][ReadStatImportColumn::readstatValueToString(key)].second;
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
		
		//Log::log() << "Setting labels for column " << col->name() << std::endl;		

		if(col->hasLabels())
		{
			//Log::log() << "It has labels" << std::endl;
			//Log::log() << "It's labelID is " << col->labelsID() << std::endl;

			const auto & valueLabelMap = _labelMap[col->labelsID()];

			//Log::log() << "It has about " << valueLabelMap.size() << std::endl;

			for(const auto & valueLabel : valueLabelMap)
			{
				readstat_type_t			type	= valueLabel.second.first;

				/*switch(type)
				{
				case READSTAT_TYPE_STRING:		Log::log() << "Type is STRING" << std::endl;	break;
				case READSTAT_TYPE_INT8:		Log::log() << "Type is INT8" << std::endl;			break;
				case READSTAT_TYPE_INT16:		Log::log() << "Type is INT16" << std::endl;			break;
				case READSTAT_TYPE_INT32:		Log::log() << "Type is INT32" << std::endl;			break;
				case READSTAT_TYPE_FLOAT:		Log::log() << "Type is FLOAT" << std::endl;			break;
				case READSTAT_TYPE_DOUBLE:		Log::log() << "Type is DOUBLE" << std::endl;		break;
				case READSTAT_TYPE_STRING_REF:	Log::log() << "Type is STRING_REF" << std::endl;	break;
				}*/

				const	std::string			&	key		= valueLabel.first;
				const	std::string			&	label	= valueLabel.second.second;

				//Log::log() << "label: '" << label << "'" << std::endl;

				switch(type)
				{
				default:
				case READSTAT_TYPE_STRING_REF:	//? I guess this won't occur because we don't accept it elsewhere
				case READSTAT_TYPE_STRING:		
					col->addLabel(key,	label);	
					break;
					
				case READSTAT_TYPE_INT8:		
				case READSTAT_TYPE_INT16:		
				case READSTAT_TYPE_INT32:		
				{
					int keyInt;
					if(ColumnUtils::convertValueToIntForImport(key, keyInt))		col->addLabel(keyInt,		label);
					else													col->addLabel(key,			label);
					break;
				}
				case READSTAT_TYPE_FLOAT:		
				case READSTAT_TYPE_DOUBLE:		
				{
					double keyDbl;
					if(ColumnUtils::convertValueToDoubleForImport(key, keyDbl))	col->addLabel(keyDbl,		label);
					else													col->addLabel(key,			label);
					break;
				}
				}
			}
		}

		//Log::log() << "Done setting labels for column " << col->name() << std::endl;
	}
}

void ReadStatImportDataSet::setCurrentRow(int row)
{
	if(_currentRow == row) return;

	_currentRow = row;

	_progressCallback(int(float(_currentRow) / float(_expectedRows) * 100.0));
}
