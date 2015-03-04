#ifndef SHAREDMEMORY_H
#define SHAREDMEMORY_H

#include <boost/interprocess/managed_shared_memory.hpp>
#include "dataset.h"

class SharedMemory
{
public:

	static DataSet *createDataSet();
	static DataSet *retrieveDataSet();
	static DataSet *enlargeDataSet(DataSet *dataSet);
	static void *deleteDataSet(DataSet *dataSet);

private:

	static std::string _memoryName;
	static boost::interprocess::managed_shared_memory *_memory;

};

#endif // SHAREDMEMORY_H
