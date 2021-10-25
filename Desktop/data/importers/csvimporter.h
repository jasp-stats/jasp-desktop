//
// Copyright (C) 2013-2018 University of Amsterdam
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

#ifndef CSVIMPORTER_H
#define CSVIMPORTER_H

#include "importer.h"
#include <QCoreApplication>
#include "utilities/qutils.h"

///
/// This description is never going to be more useful than the name of the class
class CSVImporter : public Importer
{
	Q_DECLARE_TR_FUNCTIONS(CSVImporter)
public:
	CSVImporter();

protected:
	ImportDataSet* loadFile(const std::string &locator, boost::function<void(int)> progressCallback) override;

private:
	JASPTIMER_CLASS(CSVImporter);
};

#endif // CSVIMPORTER_H
