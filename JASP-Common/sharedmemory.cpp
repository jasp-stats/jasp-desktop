#include "sharedmemory.h"

SharedMemory::SharedMemory()
{
}

bool SharedMemory::isCreatedRW()
{
	return _isCreated;
}

boost::interprocess::managed_shared_memory *SharedMemory::_memory;

bool SharedMemory::_isCreated = false;

boost::interprocess::managed_shared_memory *SharedMemory::createRW()
{
	//if (_memory != NULL)
	//	qDebug() << "SharedMemory::create(), memory already created.";

	boost::interprocess::shared_memory_object::remove("bruce");
	_memory = new boost::interprocess::managed_shared_memory(boost::interprocess::create_only, "bruce", 6553600);
	_isCreated = true;

	return _memory;
}

boost::interprocess::managed_shared_memory *SharedMemory::grow(int amount)
{
	boost::interprocess::managed_shared_memory::grow("bruce", amount);
	_memory = new boost::interprocess::managed_shared_memory(boost::interprocess::open_only, "bruce");

	return _memory;
}

boost::interprocess::managed_shared_memory *SharedMemory::get()
{
	if (_memory == NULL)
		_memory = new boost::interprocess::managed_shared_memory(boost::interprocess::open_read_only, "bruce");

	return _memory;
}
