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

#include "excelimporter.h"
#include "data/importers/excel/excel.h"
#include "data/importers/excel/excelimportcolumn.h"
#include "utilities/qutils.h"
#include <columnutils.h>
#include <string>
#include <QFileInfo>
#include <QDebug>


ExcelImporter::ExcelImporter() : Importer() {}

ImportDataSet* ExcelImporter::loadFile(const std::string &locator, std::function<void(int)> progressCallback)
{
	JASPTIMER_RESUME(ExcelImporter::loadFile);

	ImportDataSet* data = new ImportDataSet(this);
	stringvec 	colNames;

	uint16_t	cols;
	uint32_t	rows;
	uint32_t	row;
	uint16_t	col;

	std::vector<ExcelImportColumn *> importColumns;

	Excel excel(locator);
	excel.open();
	progressCallback(3);

	excel.openWorkbook();
	progressCallback(5);

	excel.selectActiveWorksheet();
	progressCallback(10);

	excel.getWorksheetDimensions(rows, cols);
	progressCallback(25);

	cols = excel.countCols();
	importColumns.reserve(cols);

	if (cols == 0)
		throw std::runtime_error(fq(tr("0 valid columns were read from the file, please check your data file.")));

	for (uint32_t row = 0; row < rows; ++row) 
	{
		stringvec lineValues;

		for (uint16_t col = 0; col < cols; ++col) 
		{
			std::string cellValue;
			excel.getCellValue(row, col, cellValue);
			lineValues.push_back(cellValue);
		}

		if (row == 0) 
		{
			colNames = lineValues;
			importColumns.reserve(colNames.size());

			for (int i = 0; i < colNames.size(); ++i) 
			{
				std::string colName = colNames[i];
				if (colName.empty()) 
					colName = "V" + std::to_string(i + 1);
				else if(ColumnUtils::isIntValue(colName) || ColumnUtils::isDoubleValue(colName))
					colName = "V" + colName;
				// distinguish duplicate column names
				if(std::find(colNames.begin(), colNames.begin() + i, colName) != colNames.begin() + i)
					colName += "_" + std::to_string(i + 1);

				colNames[i] = colName;
				importColumns.push_back(new ExcelImportColumn(data, colName, rows - 1));
			}
		}
		else 
		{
			for (int i = 0; i < importColumns.size(); ++i) 
				importColumns[i]->addValue(i < lineValues.size() ? lineValues[i] : "");
		}
	}

	for (ExcelImportColumn* col : importColumns) 
		data->addColumn(col);

	data->buildDictionary();
	excel.close();

	JASPTIMER_STOP(ExcelImporter::loadFile);

	return data;
}
