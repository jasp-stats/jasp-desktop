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

#include "rdataimporter.h"
#include "data/importers/rdata/readrdata.h"
#include "data/importers/rdata/rdataimportcolumn.h"
#include "utilities/qutils.h"
#include <columnutils.h>
#include <string>
#include <QFileInfo>
#include <QDebug>

RDataImporter::~RDataImporter() {}

ImportDataSet *RDataImporter::loadFile(const std::string &locator, std::function<void(int)> progressCallback)
{
	JASPTIMER_RESUME(RDataImporter::loadFile);

	ImportDataSet *data = new ImportDataSet(this);

	std::vector<RDataImportColumn *> importColumns;

	progressCallback(5);

	RDataReader reader(locator);
	reader.open();

	size_t rowCount, colCount;
	stringvec colNames;

	rowCount = reader.getRowCount();
	colCount = reader.getColCount();

	colNames = reader.getColumnNames();
	const auto &columns = reader.getColData();

	progressCallback(25);

	if (colCount == 0)
		throw std::runtime_error(fq(tr("0 valid columns were read from the file, please check your data file.")));

	importColumns.reserve(colNames.size());

	for (size_t colIndex = 0; colIndex < colCount; ++colIndex)
	{
		if (colIndex >= colNames.size() || colIndex >= columns.size())
			throw std::runtime_error("Column names or data mismatch.");

		RDataImportColumn *importColumn = new RDataImportColumn(data, colNames[colIndex], rowCount);

		for (size_t rowIndex = 0; rowIndex < rowCount; ++rowIndex)
		{
			if (static_cast<size_t>(rowIndex) >= columns[colIndex].size())
				throw std::runtime_error("Row data is out of bounds for column.");

			importColumn->addValue(columns[colIndex][rowIndex]);
		}

		importColumns.push_back(importColumn);
	}

	for (RDataImportColumn *col : importColumns)
		data->addColumn(col);

	progressCallback(100);

	data->buildDictionary();

	JASPTIMER_STOP(RDataImporter::loadFile);

	return data;
}
