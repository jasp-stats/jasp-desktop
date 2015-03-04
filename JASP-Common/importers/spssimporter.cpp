
#include "spssimporter.h"

#include "sharedmemory.h"
#include "dataset.h"

using namespace std;

DataSet *SPSSImporter::loadDataSet(const string &locator, boost::function<void (const string &, int)> progress)
{
	(void)locator;
	(void)progress;

	return SharedMemory::createDataSet();
}
