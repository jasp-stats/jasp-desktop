//
// Copyright (C) 2013-2025 University of Amsterdam
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

#include "rdataimportdataset.h"
#include "log.h"
#include "columnutils.h"

// According to https://teuder.github.io/rcpp4everyone_en/240_na_nan_inf.html#points-to-note-when-handling-na-with-rcpp:
#ifndef NA_INTEGER
#define NA_INTEGER (-2147483647 - 1)
#endif

RDataImportDataSet::RDataImportDataSet(RDataImporter *importer, const std::string &locator)
: ImportDataSet(static_cast<Importer*>(importer))
{
	_filePath = locator;
	open();
}

void RDataImportDataSet::open()
{
	if (_filePath.empty())
		throw std::runtime_error("File path cannot be empty.");

	_parser = rdata_parser_init();

	rdata_set_table_handler(_parser, 			&_tableHandler);
	rdata_set_column_handler(_parser, 			&_columnHandler);
	rdata_set_text_value_handler(_parser, 		&_textValueHandler);
	rdata_set_column_name_handler(_parser, 		&_columnNameHandler);
	rdata_set_error_handler(_parser, 			&_errorHandler);
	rdata_set_value_label_handler(_parser, 		&_valueLabelHandler);

	rdata_error_t result = rdata_parse(_parser, _filePath.c_str(), this);

	if (_parser)
		rdata_parser_free(_parser);
	
	if (result != RDATA_OK)
		throw std::runtime_error("Failed to parse file");
}

int RDataImportDataSet::_tableHandler(const char *name, void *ctx)
{
	RDataImportDataSet *context = static_cast<RDataImportDataSet *>(ctx);
	return context->tableHandler(name ? name : "");
}

int RDataImportDataSet::tableHandler(const std::string & name)
{
	_tableName = name;
	
	Log::log() << "Table Name: " << _tableName << std::endl;

	return 0;
}

int RDataImportDataSet::_columnHandler(const char *name, rdata_type_t type, void *data, long count, void *ctx)
{
	RDataImportDataSet *context = static_cast<RDataImportDataSet *>(ctx);
	// Log::log() << "Column name: " << name << ", Type: " << type << ", Count: " << count << std::endl;
	return context->columnHandler(name ? name : "", type, data, count);
}

int RDataImportDataSet::columnHandler(const std::string & name, rdata_type_t type, void *data, long count)
{
	Log::log() << "columnHandler name: " << name << ", Type: " << type << ", Count: " << count << std::endl;
	
	columnType	colType		= columnType::unknown;
	bool		hasLevels	= _levels.size();
	
	switch (type)
	{
	case RDATA_TYPE_INT32:
		colType = hasLevels ? columnType::ordinal : columnType::scale;
		break;
		
	case RDATA_TYPE_LOGICAL:
	case RDATA_TYPE_STRING:
		colType = columnType::nominal;
		break;
		
	case RDATA_TYPE_REAL:
	case RDATA_TYPE_TIMESTAMP:
		colType = columnType::scale;
		break;
	}
	

	_curCol = new RDataImportColumn(this, name, _levels, colType);
	_levels.clear();

	_columns.push_back(static_cast<ImportColumn*>(_curCol));

	switch (type)
	{
	case RDATA_TYPE_INT32:
	case RDATA_TYPE_LOGICAL:
	{
		int32_t *values = static_cast<int32_t *>(data);
		for (size_t i = 0; i < count; i++)
			_curCol->addValue(values[i] == NA_INTEGER ? "" : std::to_string(values[i]));
		break;
	}
	case RDATA_TYPE_REAL:
	case RDATA_TYPE_TIMESTAMP:
	{
		double *values = static_cast<double *>(data);
		for (size_t i = 0; i < count; i++)
			_curCol->addValue(std::isnan(values[i]) ? "" : ColumnUtils::doubleToStringMaxPrec(values[i]));
		break;
	}
	//Maybe works, maybe doesnt:
	//case RDATA_TYPE_DATE:
	//{
	//	tm *values = static_cast<tm *>(data);
	//	for (size_t i = 0; i < count; i++)
	//		_curCol->addValue(std::asctime(values[i]));
	//
	//	break;
	//}
	default:
		Log::log() << "Unsupported data type for column: " << name << std::endl;
		break;
	}

	return 0;
}

int RDataImportDataSet::_columnNameHandler(const char *value, int index, void *ctx)
{
	RDataImportDataSet *context = static_cast<RDataImportDataSet *>(ctx);

	return context->columnNameHandler(value ? value : "", index);
}

//Called after columnHandler apparently
int RDataImportDataSet::columnNameHandler(const std::string & name, int index)
{
	_columns[index]->setName(name);

	return 0;
}

void RDataImportDataSet::_errorHandler(const char *error_message, void *ctx)
{
	Log::log() << "Error: " << error_message << std::endl;
}

int RDataImportDataSet::_textValueHandler(const char *value, int index, void *ctx)
{
	// This handled if data type in _columnHandler is "RDATA_TYPE_STRING", because it's empty!
	RDataImportDataSet *context = static_cast<RDataImportDataSet *>(ctx);

	return context->textValueHandler(value ? value : "", index);
}

int RDataImportDataSet::textValueHandler(const std::string & value, int index)
{
	if (!_curCol)
	{
		Log::log() << "Error: textValueHandler called before columnHandler." << std::endl;
		return 1; // Abort processing
	}

	_curCol->addValue(value, index);
	
	return 0;
}

int RDataImportDataSet::_valueLabelHandler(const char *value, int index, void *ctx)
{
	RDataImportDataSet *context = static_cast<RDataImportDataSet *>(ctx);
	return context->valueLabelHandler(value ? value : "", index);
}	
	
int RDataImportDataSet::valueLabelHandler(const std::string & level, int index)
{
	if(_levels.size() <= index)
		_levels.resize(index+1);

	_levels[index] = level;

	return 0;
}
