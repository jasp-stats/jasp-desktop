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
	Log::log() << "SharedMemory::retrieveDataSet(" <<  ProcessInfo::currentPID() << ")" << std::endl;

	if (_memory == NULL)
	{
		stringstream ss;
		ss << "JASP-DATA-";
		ss << ProcessInfo::currentPID();
		_memoryName = ss.str();

		interprocess::shared_memory_object::remove(_memoryName.c_str());
		_memory = new interprocess::managed_shared_memory(interprocess::create_only, _memoryName.c_str(), 6 * 1024 * 1024);
		Log::log() << "Created shared mem with name " << _memoryName << std::endl;
	}

	DataSet * data = _memory->construct<DataSet>(interprocess::unique_instance)(_memory);
	Log::log() << "(Re)created dataset in shared mem with name " << _memoryName << std::endl;
	return data;
}

DataSet *SharedMemory::retrieveDataSet(unsigned long parentPID)
{
	Log::log() << "SharedMemory::retrieveDataSet(" << parentPID << ")" << std::endl;

	DataSet * data = nullptr;
	try
	{
		if (_memory == nullptr)
		{
			if(parentPID == 0)
				parentPID = ProcessInfo::parentPID();

			_memoryName = "JASP-DATA-" + std::to_string(parentPID);
			_memory		= new interprocess::managed_shared_memory(interprocess::open_only, _memoryName.c_str());
			Log::log() << "Opened shared mem with name " << _memoryName << std::endl;
		}

		data = _memory->find<DataSet>(interprocess::unique_instance).first;
	}
	catch (const interprocess::interprocess_exception& e)
	{
		Log::log() << "Error when retrieving the data set: " << e.what() << std::endl;
		data = nullptr;
	}

	if (data == nullptr)
		Log::log() << "No data set found in shared memory with name: " << _memoryName << std::endl;

	return data;
}

DataSet *SharedMemory::enlargeDataSet(DataSet *)
{
	size_t extraSize = _memory->get_size();

	Log::log() << "SharedMemory::enlargeDataSet(" << _memoryName << ") to " << extraSize << std::endl;

	delete _memory;

	interprocess::managed_shared_memory::grow(_memoryName.c_str(), extraSize);
	_memory = new interprocess::managed_shared_memory(interprocess::open_only, _memoryName.c_str());

	DataSet *dataSet = retrieveDataSet();
	dataSet->setSharedMemory(_memory);

	return dataSet;
}

void SharedMemory::deleteDataSet(DataSet *dataSet)
{
	Log::log() << "SharedMemory::deleteDataSet " << _memoryName << std::endl;
	_memory->destroy_ptr(dataSet);
}

void SharedMemory::unloadDataSet()
{
	Log::log() << "SharedMemory::unloadDataSet " << _memoryName << ( _memory ? "" : " but it wasn't loaded.") << std::endl;
	delete _memory;
	_memory = nullptr;
}
