//
// Copyright (C) 2013-2024 University of Amsterdam
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
// Author: Shun Wang
//


#ifndef MWX_H
#define MWX_H

#include <string>
#include <vector>
#include <json/json.h>
#include "minitabimportcolumn.h"


class Minitab
{
public:
	Minitab(const std::string &path);

	void						parseMwx();
	void						getColumns(std::vector<MwxImportColumn*> &outColumns, ImportDataSet* dataSet);

	uint32_t				getRowCount() const { return _numRows; }
	uint16_t				getColCount() const { return _numCols; }

private:
	std::string			_path;
	Json::Value			_sheetRoot,  // root/0/sheet.json
										_metadata;  // metadata such (sheet_metadata_20.json)

		
	stringvec				_levels;
		
	uint32_t				_numRows;
	uint16_t				_numCols;
		
	std::string			findMetadataPath();

	Json::Value			readJsonFromArchive(const std::string &entryPath);
};

#endif // MWX_H
