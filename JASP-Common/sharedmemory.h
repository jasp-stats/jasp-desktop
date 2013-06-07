#ifndef SHAREDMEMORY_H
#define SHAREDMEMORY_H

#include <boost/interprocess/managed_shared_memory.hpp>

class SharedMemory
{
public:
	SharedMemory();

	static boost::interprocess::managed_shared_memory *create();
	static boost::interprocess::managed_shared_memory *get();

private:
	static boost::interprocess::managed_shared_memory *_memory;
};

#endif // SHAREDMEMORY_H
