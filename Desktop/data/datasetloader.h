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

#ifndef DATASETLOADER_H
#define DATASETLOADER_H

#include "dataset.h"
#include <boost/function.hpp>
#include "datasetpackage.h"
#include "importers/importer.h"

/// Helper class for loading or synchronizing with data/jasp-files
/// It can determine which importer is needed for which file-extension as needed
class DataSetLoader
{
public:
	static void loadPackage(const std::string & locator, const std::string & extension, boost::function<void (int progress)> progress = nullptr);
	static void syncPackage(const std::string & locator, const std::string & extension, boost::function<void (int progress)> progress = nullptr);
	static void freeDataSet(DataSet *dataSet);

	static std::string getExtension(const std::string &locator, const std::string &extension);

private:
	static Importer* getImporter(const std::string &locator, const std::string &extension);
};

#endif // DATASETLOADER_H
