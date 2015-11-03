#ifndef SHAREDMEMORY_H
#define SHAREDMEMORY_H

#include <boost/interprocess/managed_shared_memory.hpp>
#include "dataset.h"

/*
 * Data sets are created in shared memory, allowing the UI process
 * and the background processes to all access to it.
 * The UI process calls createDataSet(), and the background
 * processes latch onto this with retrieveDataSet()
 * A DataSet needs to be able to allocate memory, so the memory
 * segment is passed into the DataSet's constructor.
 * This allows the DataSet to allocate child objects (such as columns)
 * in shared memory as well.
 * Good examples of creating and populating a DataSet can be found
 * in the importers
 */

class SharedMemory
{
public:

	static DataSet *createDataSet();
	static DataSet *retrieveDataSet();
	static DataSet *enlargeDataSet(DataSet *dataSet);
	static void deleteDataSet(DataSet *dataSet);

private:

	static std::string _memoryName;
	static boost::interprocess::managed_shared_memory *_memory;

};

#endif // SHAREDMEMORY_H
