#include "datasetloader.h"

#include <boost/filesystem.hpp>

#include "sharedmemory.h"
#include "dataset.h"

#include "importers/csvimporter.h"
#include "importers/spssimporter.h"
#include "importers/jaspimporter.h"

using namespace boost::interprocess;
using namespace boost;
using namespace std;


void DataSetLoader::loadPackage(DataSetPackage *packageData, const string &locator, boost::function<void(const string &, int)> progress)
{
	filesystem::path path(locator);

	if (path.extension().compare(string(".sav")) == 0)
		packageData->dataSet = SPSSImporter::loadDataSet(locator, progress);
	else if (path.extension().compare(string(".csv")) == 0)
		packageData->dataSet = CSVImporter::loadDataSet(locator, progress);
	else
		JASPImporter::loadDataSet(packageData, locator, progress);
}

void DataSetLoader::freeDataSet(DataSet *dataSet)
{
	SharedMemory::deleteDataSet(dataSet);
}

DataSet *DataSetLoader::getDataSet()
{
	return SharedMemory::retrieveDataSet();
}


