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

#include "log.h"

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
	DataSet * data = nullptr;
	try
	{
		if (_memory == nullptr)
		{
			if(parentPID == 0)
				parentPID = ProcessInfo::parentPID();

			_memoryName = "JASP-DATA-" + std::to_string(parentPID);
			_memory		= new interprocess::managed_shared_memory(interprocess::open_only, _memoryName.c_str());
		}

		data = _memory->find<DataSet>(interprocess::unique_instance).first;
	}
	catch (const interprocess::interprocess_exception& e)
	{
		data = nullptr;

	}

	return data;
}

DataSet *SharedMemory::enlargeDataSet(DataSet *)
{
	size_t extraSize = _memory->get_size();

	Log::log() << "SharedMemory::enlargeDataSet to " << extraSize << std::endl;

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

void SharedMemory::unloadDataSet()
{
	if(_memory != NULL)
		delete _memory;

	_memory = NULL;
}
