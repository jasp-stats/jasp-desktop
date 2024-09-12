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

#ifndef EXCELIMPORTER_H
#define EXCELIMPORTER_H

#include "importer.h"
#include <QCoreApplication>
#include "timers.h"


class ExcelImporter : public Importer
{
	Q_DECLARE_TR_FUNCTIONS(ExcelImporter)
public:
	ExcelImporter();
	virtual ~ExcelImporter() {}

protected:
	ImportDataSet* loadFile(const std::string &locator, std::function<void(int)> progressCallback) override;

private:
	JASPTIMER_CLASS(ExcelImporter);
};

#endif // EXCELIMPORTER_H
