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

#include "readrdata.h"
#include "log.h"

RDataReader::RDataReader(const std::string &locator)
{
	_filePath = locator;
}

void RDataReader::open()
{
	if (_filePath.empty())
		throw std::runtime_error("File path cannot be empty.");

	_parser = rdata_parser_init();

	rdata_set_table_handler(_parser, &_tableHandler);
	rdata_set_column_handler(_parser, &_columnHandler);
	rdata_set_text_value_handler(_parser, &_textValueHandler);
	rdata_set_column_name_handler(_parser, &_columnNameHandler);
	rdata_set_error_handler(_parser, &_errorHandler);
	// rdata_set_value_label_handler(_parser, &_valueLabelHandler);

	rdata_error_t result = rdata_parse(_parser, _filePath.c_str(), &_context);

	_rowCount = _context.row_count;
	_colCount = _context.column_count;
	_columnNames = _context.column_names;
	_column_data = _context.column_data;

	if (result != RDATA_OK)
		throw std::runtime_error("Failed to parse file");

	if (_parser)
		rdata_parser_free(_parser);
}

int RDataReader::_tableHandler(const char *name, void *ctx)
{
	RDataCtx *context = static_cast<RDataCtx *>(ctx);

	if (!name)
		context->table_name = "Default_table";
	else
		context->table_name = name;

	Log::log() << "Table Name: " << ( !name ? "nullptr" : name) << std::endl;

	return 0;
}

int RDataReader::_columnHandler(const char *name, rdata_type_t type, void *data, long count, void *ctx)
{

	RDataCtx *context = static_cast<RDataCtx *>(ctx);
	// Log::log() << "Column name: " << name << ", Type: " << type << ", Count: " << count << std::endl;

	context->column_count++;

	if (count > context->row_count)
	{
		context->row_count = count;
	}

	stringvec column_values;

	if (type == RDATA_TYPE_STRING)
	{
		// Initialize an empty vector for string type to be filled in _textValueHandler
		context->column_data.emplace_back(stringvec(count, ""));
	}
	else
	{
		switch (type)
		{
		case RDATA_TYPE_INT32:
		case RDATA_TYPE_LOGICAL:
		{
			int32_t *values = static_cast<int32_t *>(data);
			for (size_t i = 0; i < count; i++)
				column_values.push_back(std::to_string(values[i]));
			break;
		}
		case RDATA_TYPE_REAL:
		{
			double *values = static_cast<double *>(data);
			for (size_t i = 0; i < count; i++)
			{
				if (std::isnan(values[i]))
					column_values.push_back("NA");
				else
					column_values.push_back(std::to_string(values[i]));
			}
			break;
		}
		default:
			Log::log() << "Unsupported data type for column: " << name << std::endl;
			break;
		}

		context->column_data.push_back(column_values);
	}

	return 0;
}

int RDataReader::_columnNameHandler(const char *value, int index, void *ctx)
{
	RDataCtx *context = static_cast<RDataCtx *>(ctx);

	if (index >= context->column_names.size())
		context->column_names.resize(index + 1);

	context->column_names[index] = value ? std::string(value) : "Column_" + std::to_string(index + 1);
	//Log::log() << "Column name : " << value << " [Index " << index << "]: " << context->column_names[index] << std::endl;

	return 0;
}

void RDataReader::_errorHandler(const char *error_message, void *ctx)
{
	Log::log() << "Error: " << error_message << std::endl;
}

int RDataReader::_textValueHandler(const char *value, int index, void *ctx)
{
	// This handled if data type in _columnHandler is "RDATA_TYPE_STRING", because it's empty!
	RDataCtx *context = static_cast<RDataCtx *>(ctx);

	if (context->column_data.empty())
	{
		Log::log() << "Error: _textValueHandler called before _columnHandler." << std::endl;
		return 1; // Abort processing
	}

	size_t column_index = context->column_data.size() - 1;
	if (index >= context->column_data[column_index].size())
	{
		context->column_data[column_index].resize(index + 1, "");
	}

	context->column_data[column_index][index] = value ? value : "NA";

	return 0;
}

int RDataReader::_valueLabelHandler(const char *value, int index, void *ctx)
{
	// TODO: implement importing factor level as label
	return 0;
}
