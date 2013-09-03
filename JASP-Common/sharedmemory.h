#ifndef SHAREDMEMORY_H
#define SHAREDMEMORY_H

#include <boost/interprocess/managed_shared_memory.hpp>

class SharedMemory
{
public:
	SharedMemory();

	static bool isCreatedRW();
	static boost::interprocess::managed_shared_memory *createRW();
	static boost::interprocess::managed_shared_memory *get();
	static boost::interprocess::managed_shared_memory *grow(int amount);
private:
	static boost::interprocess::managed_shared_memory *_memory;
	static bool _isCreated;
};

#endif // SHAREDMEMORY_H
