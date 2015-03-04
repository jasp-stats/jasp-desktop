#include "datasetloader.h"

#include <boost/filesystem.hpp>

#include "sharedmemory.h"
#include "dataset.h"

#include "importers/csvimporter.h"
#include "importers/spssimporter.h"

using namespace boost::interprocess;
using namespace boost;
using namespace std;


DataSet* DataSetLoader::loadDataSet(const string &locator, boost::function<void(const string &, int)> progress)
{
	filesystem::path path(locator);

	if (path.extension().compare(".sav") == 0)
		return SPSSImporter::loadDataSet(locator, progress);
	else
		return CSVImporter::loadDataSet(locator, progress);
}

void DataSetLoader::freeDataSet(DataSet *dataSet)
{
	SharedMemory::deleteDataSet(dataSet);
}

DataSet *DataSetLoader::getDataSet()
{
	return SharedMemory::retrieveDataSet();
}


