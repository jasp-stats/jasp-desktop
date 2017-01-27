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

#include "datasetloader.h"

#include <boost/filesystem.hpp>
#include <boost/algorithm/string.hpp>

#include "sharedmemory.h"
#include "dataset.h"

#include "importers/csvimporter.h"
#include "importers/spssimporter.h"
#include "importers/jaspimporter.h"
#include "importers/odsimporter.h"

#include <QFileInfo>
#include <QSettings>

using namespace std;
using namespace spss;
using namespace boost::interprocess;
using namespace boost;

string DataSetLoader::getExtension(const string &locator, const string &extension) {
	filesystem::path path(locator);
	string ext = path.extension().generic_string();

	if (!ext.length()) ext=extension;
	return ext;
}

Importer* DataSetLoader::getImporter(DataSetPackage *packageData, const string &locator, const string &extension)
{
	Importer* result = NULL;
	string ext = getExtension(locator, extension);

	if (boost::iequals(ext,".csv") || boost::iequals(ext,".txt"))
		result = new CSVImporter(packageData);
	else if (boost::iequals(ext,".sav"))
		result = new SPSSImporter(packageData);
	else if (boost::iequals(ext,".ods"))
		result = new ODSImporter(packageData);

	return result;
}

void DataSetLoader::loadPackage(DataSetPackage *packageData, const string &locator, const string &extension, boost::function<void(const string &, int)> progress)
{
	Importer* importer = getImporter(packageData, locator, extension);

	if (importer)
	{
		importer->loadDataSet(locator, progress);
		delete importer;
	}
	else
		JASPImporter::loadDataSet(packageData, locator, progress);
}

void DataSetLoader::syncPackage(DataSetPackage *packageData, const string &locator, const string &extension, boost::function<void(const string &, int)> progress)
{
	Importer* importer = getImporter(packageData, locator, extension);

	if (importer)
	{
		importer->syncDataSet(locator, progress);
		delete importer;
	}
}

void DataSetLoader::freeDataSet(DataSet *dataSet)
{
	SharedMemory::deleteDataSet(dataSet);
}

