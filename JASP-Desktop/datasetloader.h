//
// Copyright (C) 2013-2017 University of Amsterdam
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

#ifndef DATASETLOADER_H
#define DATASETLOADER_H

#include "dataset.h"
#include <boost/function.hpp>
#include "datasetpackage.h"
#include "importers/importer.h"

class DataSetLoader
{
public:
	static void loadPackage(DataSetPackage *packageData, const std::string &locator, const std::string &extension, boost::function<void (const std::string &stage, int progress)> progress = NULL);
	static void syncPackage(DataSetPackage *packageData, const std::string &locator, const std::string &extension, boost::function<void (const std::string &, int)> progress = NULL);
	static void freeDataSet(DataSet *dataSet);

private:
	static std::string getExtension(const std::string &locator, const std::string &extension);
	static Importer* getImporter(DataSetPackage *packageData, const std::string &locator, const std::string &extension);
};

#endif // DATASETLOADER_H
