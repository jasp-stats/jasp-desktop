#ifndef SHAREDMEMORY_H
#define SHAREDMEMORY_H

#include <boost/interprocess/managed_shared_memory.hpp>

class SharedMemory
{
public:

	static boost::interprocess::managed_shared_memory *get(bool master = false);
	static boost::interprocess::managed_shared_memory *grow(int amount);

private:

	static std::string _memoryName;
	static boost::interprocess::managed_shared_memory *_memory;

};

#endif // SHAREDMEMORY_H
