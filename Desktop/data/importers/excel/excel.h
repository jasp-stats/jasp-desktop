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

#ifndef EXCEL_H
#define EXCEL_H

#include <vector>
#include <string>
#include <stdint.h>

#include "freexl.h"

class Excel
{
public:
	Excel(const std::string &path);

	void		open();
	void		close();

	bool		readLine(std::vector<std::string> &items);
	long		pos();
	long		size();
	uint16_t	countCols();

	void		openWorkbook();
	void		selectActiveWorksheet();
	void		getWorksheetDimensions(uint32_t &rows, uint16_t &cols);
	void		getCellValue(uint32_t &row, uint16_t &col, std::string &cellValue);

private:

	long		_fileSize;
	long		_filePosition;
	uint16_t	_numCols;

private:

	std::string		_path;
	unsigned int	_fileInfo;
	const void		*_handle;
};

#endif // EXCEL_H
