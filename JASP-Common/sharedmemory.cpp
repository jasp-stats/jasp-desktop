
#include "sharedmemory.h"

#include "process.h"

#include <sstream>

boost::interprocess::managed_shared_memory *SharedMemory::_memory;
std::string SharedMemory::_memoryName;

boost::interprocess::managed_shared_memory *SharedMemory::grow(int amount)
{	
	boost::interprocess::managed_shared_memory::grow(_memoryName.c_str(), amount);
	_memory = new boost::interprocess::managed_shared_memory(boost::interprocess::open_only, _memoryName.c_str());

	return _memory;
}

boost::interprocess::managed_shared_memory *SharedMemory::get(bool master)
{
	if (_memory == NULL)
	{
		std::stringstream ss;
		ss << "JASP-DATA-";

		if (master)
		{   
			ss << Process::currentPID();
			_memoryName = ss.str();
            boost::interprocess::shared_memory_object::remove(_memoryName.c_str());
            _memory = new boost::interprocess::managed_shared_memory(boost::interprocess::create_only, _memoryName.c_str(), 6553600);
		}
		else
		{
			ss << Process::parentPID();
			_memoryName = ss.str();
			_memory = new boost::interprocess::managed_shared_memory(boost::interprocess::open_read_only, _memoryName.c_str());
		}


	}

	return _memory;
}
