#include "sharedmemory.h"

SharedMemory::SharedMemory()
{
}

boost::interprocess::managed_shared_memory *SharedMemory::_memory;

boost::interprocess::managed_shared_memory *SharedMemory::create()
{
    //if (_memory != NULL)
    //	qDebug() << "SharedMemory::create(), memory already created.";

	boost::interprocess::shared_memory_object::remove("bruce");
	_memory = new boost::interprocess::managed_shared_memory(boost::interprocess::create_only, "bruce", 6553600);

	return _memory;
}

boost::interprocess::managed_shared_memory *SharedMemory::get()
{
	if (_memory == NULL)
		_memory = new boost::interprocess::managed_shared_memory(boost::interprocess::open_read_only, "bruce");

	return _memory;
}
