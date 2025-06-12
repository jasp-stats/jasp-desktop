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
#include "../importdataset.h"
#include "../rdataimporter.h"
#include "rdataimportcolumn.h"
#include "rdata.h"

class RDataImportDataSet : public ImportDataSet
{
public:
	RDataImportDataSet(RDataImporter *importer, const std::string &_filePath);
	
protected:	
	void 			open();
	
private:
	static 	int		_tableHandler(		const char *name, 														void *ctx);
	static 	int 	_columnHandler(		const char *name, 			rdata_type_t type, void *data, long count, 	void *ctx);
	static 	int 	_columnNameHandler(	const char *value, 			int index, 									void *ctx);
	static 	int 	_textValueHandler(	const char *value, 			int index, 									void *ctx);
	static 	int 	_valueLabelHandler(	const char *value, 			int index, 									void *ctx);
	static 	void	_errorHandler(		const char *error_message, 												void *ctx);

			int		tableHandler(		const std::string & name);
			int 	columnHandler(		const std::string & name, 			rdata_type_t type, void *data, long count);
			int 	columnNameHandler(	const std::string & value, 			int index);
			int 	textValueHandler(	const std::string & value, 			int index);
			int 	valueLabelHandler(	const std::string & value, 			int index);
			void	errorHandler(		const std::string & error_message);

	std::string 			_filePath;
	rdata_parser_t 		*	_parser;
	std::string				_tableName = "";
	RDataImportColumn	*	_curCol = nullptr;
	stringvec				_levels; //Levels are passed before column_handler, which is where we actually make the columns. So store it here
};

#endif // RDATA_H
