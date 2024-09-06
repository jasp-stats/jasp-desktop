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

#include "excel.h"
#include "utilities/qutils.h"
#include "utils.h"

#include <QFileInfo>
#include <QDebug>

using namespace std;

Excel::Excel(const string &locator)
{
	_path = locator;
}

void Excel::open()
{
	_fileSize = QFileInfo::size(_path);

	if (_fileSize < 0)
		throw runtime_error("Could not access file");

	if (_fileSize == 0)
		throw runtime_error("File is empty");
}
void Excel::openWorkbook() 
{
	QString xlsFilePath = tq(_path);
	const char* utf8Path = _path.c_str(); //But it would be better to just use _path.c_str() directly if you need it. It is in utf8 in any case.
	QString extension = QFileInfo(xlsFilePath).suffix().toLower();

	int ret = 0;
	if (extension == "xls")
		ret = freexl_open(utf8Path, &_handle);
	else if (extension == "xlsx")
		ret = freexl_open_xlsx(utf8Path, &_handle);
	else
		throw runtime_error("Unsupported file format: " + fq(extension));

	if(ret != FREEXL_OK)
		throw runtime_error("Unexpected error while loading excel file, error code: " + std::to_string(ret));
}

void Excel::selectActiveWorksheet() 
{
	int ret = freexl_select_active_worksheet(_handle, 0);  // import the first worksheet(index=0) by default.
	if (ret != FREEXL_OK)
		throw runtime_error("Could not select active worksheet,\n error code: " + std::to_string(ret));
}

void Excel::getWorksheetDimensions(uint32_t &rows, uint16_t &cols) {
	int ret = freexl_worksheet_dimensions(_handle, &rows, &cols);

	if (ret != FREEXL_OK)
		throw runtime_error("Could not read worksheet dimensions, error code: " + std::to_string(ret));

	_numCols = cols; //get cols count while read sheet
}

void Excel::getCellValue(uint32_t &row, uint16_t &col, std::string &cellValue) 
{
	FreeXL_CellValue cell;
	int ret = freexl_get_cell_value(_handle, row, col, &cell);

	if (ret != FREEXL_OK)
		cellValue = "ERROR " + std::to_string(ret);

	switch (cell.type)
	{
	case FREEXL_CELL_TEXT:
	case FREEXL_CELL_SST_TEXT:
	case FREEXL_CELL_DATE:  // So we store it as a character for now until support for date types.
	case FREEXL_CELL_DATETIME:
	case FREEXL_CELL_TIME:
		cellValue = cell.value.text_value;
		break;
	case FREEXL_CELL_INT:
		cellValue = std::to_string(cell.value.int_value);
		break;
	case FREEXL_CELL_DOUBLE:
		cellValue = std::to_string(cell.value.double_value);
		break;
	case FREEXL_CELL_NULL:
	default:
		cellValue = "";
		break;
	}
}

uint16_t Excel::countCols()
{
	return _numCols;
}

void Excel::close() 
{
	if (_handle)
	{
		freexl_close(_handle);
		_handle = nullptr;
	}
}
