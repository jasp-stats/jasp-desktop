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

#ifndef RDATA_H
#define RDATA_H

#include <string>
#include <rdata.h>
#include "utils.h"

class RDataReader
{
public:
	RDataReader(const std::string &_filePath);

	void 			open();
	size_t 			getRowCount() 		const { return _rowCount; 	}
	size_t 			getColCount() 		const { return _colCount; 	}
	stringvec 		getColumnNames() 	const { return _columnNames;}
	stringvecvec 	getColData() 		const { return _column_data;}

private:
	struct RDataCtx
	{
		size_t 		column_count = 0;
		size_t 		row_count = 0;
		const char *table_name = nullptr;
		stringvec 	column_names;
		stringvecvec column_data;
	};

	static int	_tableHandler(const char *name, void *ctx);
	static int 	_columnHandler(const char *name, rdata_type_t type, void *data, long count, void *ctx);
	static int 	_columnNameHandler(const char *value, int index, void *ctx);
	static int 	_textValueHandler(const char *value, int index, void *ctx);
	static int 	_valueLabelHandler(const char *value, int index, void *ctx);
	static void	_errorHandler(const char *error_message, void *ctx);

	std::string 	_filePath;
	rdata_parser_t *_parser;

	RDataCtx 		_context;
	stringvec 		_columnNames;
	size_t 			_rowCount;
	size_t 			_colCount;
	stringvecvec 	_column_data;
};

#endif // RDATA_H
