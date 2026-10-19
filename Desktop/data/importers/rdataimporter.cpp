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
#include "rdata/rdataimportdataset.h"
#include "qutils.h"
#include <columnutils.h>
#include <string>
#include <QFileInfo>
#include <QDebug>

RDataImporter::~RDataImporter() {}

ImportDataSet *RDataImporter::loadFile(const std::string &locator, std::function<void(int)> progressCallback)
{
	JASPTIMER_RESUME(RDataImporter::loadFile);

	//To do: better progress callback :p
	progressCallback(5);
	RDataImportDataSet *data = new RDataImportDataSet(this, locator);
	progressCallback(75);

	if (data->columnCount() == 0)
		throw std::runtime_error(fq(tr("0 valid columns were read from the file, please check your data file.")));

	progressCallback(100);

	data->buildDictionary();

	JASPTIMER_STOP(RDataImporter::loadFile);

	return data;
}
