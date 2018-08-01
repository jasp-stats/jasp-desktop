//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

#include "sharedmemory.h"

#include "processinfo.h"
#include "tempfiles.h"

#include <sstream>
#include <iostream>

using namespace std;
using namespace boost;

interprocess::managed_shared_memory *SharedMemory::_memory = NULL;
string SharedMemory::_memoryName;

DataSet *SharedMemory::createDataSet()
{
	if (_memory == NULL)
	{
		stringstream ss;
		ss << "JASP-DATA-";
		ss << ProcessInfo::currentPID();
		_memoryName = ss.str();

		TempFiles::addShmemFileName(_memoryName);

		interprocess::shared_memory_object::remove(_memoryName.c_str());
		_memory = new interprocess::managed_shared_memory(interprocess::create_only, _memoryName.c_str(), 6 * 1024 * 1024);
	}

	DataSet * data = _memory->construct<DataSet>(interprocess::unique_instance)(_memory);
	return data;
}

DataSet *SharedMemory::retrieveDataSet(unsigned long parentPID)
{
	if (_memory == NULL)
	{
		if(parentPID == 0)
			parentPID = ProcessInfo::parentPID();

		stringstream ss;
		ss << "JASP-DATA-";
		ss << parentPID;
		_memoryName = ss.str();

		_memory = new interprocess::managed_shared_memory(interprocess::open_only, _memoryName.c_str());
	}

	DataSet * data = _memory->find<DataSet>(interprocess::unique_instance).first;
	return data;
}

DataSet *SharedMemory::enlargeDataSet(DataSet *)
{
	size_t extraSize = _memory->get_size();

#ifdef JASP_DEBUG
	std::cout << "SharedMemory::enlargeDataSet to " << extraSize << std::endl;
#endif

	delete _memory;

	interprocess::managed_shared_memory::grow(_memoryName.c_str(), extraSize);
	_memory = new interprocess::managed_shared_memory(interprocess::open_only, _memoryName.c_str());

	DataSet *dataSet = retrieveDataSet();
	dataSet->setSharedMemory(_memory);

	return dataSet;
}

void SharedMemory::deleteDataSet(DataSet *dataSet)
{
	_memory->destroy_ptr(dataSet);
}
