
#include "spssimporter.h"

#include "sharedmemory.h"
#include "dataset.h"

using namespace std;

void SPSSImporter::loadDataSet(DataSetPackage *packageData, const string &locator, boost::function<void (const string &, int)> progress)
{
	(void)locator;
	(void)progress;

	packageData->dataSet = SharedMemory::createDataSet(); // this is required incase the loading of the data fails so that the SharedMemory::createDataSet() can be later freed.

}
