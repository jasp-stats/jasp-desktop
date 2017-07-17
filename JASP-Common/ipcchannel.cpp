//
// Copyright (C) 2013-2017 University of Amsterdam
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

#include "ipcchannel.h"

#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/nowide/convert.hpp>

using namespace std;
using namespace boost;
using namespace boost::posix_time;

IPCChannel::IPCChannel(std::string name, int channelNumber, bool isSlave)
{
	_name = name;
	_channelNumber = channelNumber;
	_isSlave = isSlave;

	if (!isSlave)
	{
		interprocess::shared_memory_object::remove(name.c_str());
		try
		{
			_memory = new interprocess::managed_windows_shared_memory(interprocess::create_only, name.c_str(), 4 * 1024 * 1024);
			std::cout << "Master: Shared memory created with name " << name.c_str() << std::endl;
		}
		catch (boost::interprocess::interprocess_exception &e)
		{
			std::cout << "Master: Cannot create shared memory with name " << name.c_str() << ": " << e.what() << std::endl;
		}
		try
		{
			boost::interprocess::managed_windows_shared_memory *test = new interprocess::managed_windows_shared_memory(interprocess::open_only, name.c_str());
			std::cout << "Master: Shared memory found with name " << name.c_str() << std::endl;
		}
		catch (boost::interprocess::interprocess_exception &e)
		{
			std::cout << "Master: Cannot find shared memory with name " << name.c_str() << ": " << e.what() << std::endl;
		}
	}
	else
	{
		try
		{
			_memory = new interprocess::managed_windows_shared_memory(interprocess::open_only, name.c_str());
			std::cout << "Slave: Shared memory read with name " << name.c_str() << std::endl;
		}
		catch (boost::interprocess::interprocess_exception &e)
		{
			std::cout << "Slave: Cannot find shared memory with name " << name.c_str() << " :"  << e.what() << std::endl;
		}

	}
	std::cout.flush();

	stringstream mutexNameMaster, mutexNameSlave;
	stringstream dataNameMaster, dataNameSlave;

	mutexNameMaster << name << "-mm-" << channelNumber;
	mutexNameSlave << name << "-ms-" << channelNumber;
	dataNameMaster << name << "-dm-" << channelNumber;
	dataNameSlave << name << "-ds-" << channelNumber;

	if (!isSlave)
	{
		//_mutexMaster  = _memory->construct<interprocess::interprocess_mutex>(mutexNameMaster.str().c_str())();
		//_mutexSlave  = _memory->construct<interprocess::interprocess_mutex>(mutexNameSlave.str().c_str())();
		_mutexMaster = new interprocess::named_mutex(interprocess::create_only, mutexNameMaster.str().c_str());
		_mutexSlave = new interprocess::named_mutex(interprocess::create_only, mutexNameSlave.str().c_str());
		_dataMaster   = _memory->construct<String>(dataNameMaster.str().c_str())(_memory->get_segment_manager());
		_dataSlave   = _memory->construct<String>(dataNameSlave.str().c_str())(_memory->get_segment_manager());
		_dataMaster->clear();
		_dataSlave->clear();
	}
	else
	{
		std::cout << "Try to find mutex" << std::endl;
		std::cout.flush();
		_mutexMaster = NULL;
		try
		{
			_mutexMaster = new interprocess::named_mutex(interprocess::open_only, mutexNameMaster.str().c_str());
			//_mutexMaster  = _memory->find<interprocess::interprocess_mutex>(mutexNameMaster.str().c_str()).first;
		}
		catch (boost::interprocess::interprocess_exception &e)
		{
			std::cout << "Slave: Cannot find mutex with name " << mutexNameMaster.str().c_str() << " :"  << e.what() << std::endl;
		}
		if (!_mutexMaster)
			std::cout << "Cannot find mutex master" << std::endl;
		else
			std::cout << "Found mutex" << std::endl;
		std::cout.flush();

		//_mutexSlave  = _memory->find<interprocess::interprocess_mutex>(mutexNameSlave.str().c_str()).first;

		std::cout << "Create _mutexSlave" << std::endl;
		std::cout.flush();
		_mutexSlave = new interprocess::named_mutex(interprocess::open_only, mutexNameSlave.str().c_str());
		std::cout << "_mutexSlave Created" << std::endl;
		std::cout.flush();
		if (!_mutexSlave)
			std::cout << "Cannot find mutex slave" << std::endl;
		std::cout.flush();

		_dataMaster = NULL;
		try
		{
			std::cout << "Find string with name " << dataNameMaster.str().c_str() << std::endl;
			_dataMaster   = _memory->find<String>(dataNameMaster.str().c_str()).first;
			std::cout << "Slave: found string with name " << dataNameMaster.str().c_str() << std::endl;
		}
		catch (boost::interprocess::interprocess_exception &e)
		{
			std::cout << "Slave: Cannot find string with name " << dataNameMaster.str().c_str() << " :"  << e.what() << std::endl;
		}
		if (!_dataMaster)
			std::cout << "Cannot find data master" << std::endl;
		_dataSlave   = _memory->find<String>(dataNameSlave.str().c_str()).first;
		if (!_dataSlave)
			std::cout << "Cannot find data slave" << std::endl;
		std::cout << "Mutex done" << std::endl;
		std::cout.flush();
	}
}

IPCChannel::~IPCChannel()
{
	interprocess::shared_memory_object::remove(_name.c_str());
}

void IPCChannel::send(string &data)
{
	if (_isSlave)
	{
		_mutexSlave->lock();
		_dataSlave->assign(data.begin(), data.end());
		_mutexSlave->unlock();
	}
	else
	{
		_mutexMaster->lock();
		std::cout << "Master send data" << std::endl;
		_dataMaster->assign(data.begin(), data.end());
		string result;
		result.assign(_dataMaster->c_str(), _dataMaster->size());
		std::cout << "Master send data: " << result << std::endl;
		std::cout.flush();
		_mutexMaster->unlock();
	}
}

bool IPCChannel::receive(string &data, int timeout)
{
	bool found = false;
	ptime now(microsec_clock::universal_time());
	ptime then = now + microseconds(1000 * timeout);
	if (_isSlave)
	{
		std::cout << "Slave try to lock to read data" << std::endl;
		if (_mutexMaster->timed_lock(then))
		{
			std::cout << "Slave received data size: " << _dataMaster->size() << std::endl;
			if (_dataMaster->size())
			{
				data.assign(_dataMaster->c_str(), _dataMaster->size());
				std::cout << "Slave data: " << data << std::endl;
				_dataMaster->clear();
				found = true;
			}
			_mutexMaster->unlock();
		}
		std::cout.flush();
	}
	else
	{
		if (_mutexSlave->timed_lock(then))
		{
			if (_dataSlave->size())
			{
				data.assign(_dataSlave->c_str(), _dataSlave->size());
				_dataSlave->clear();
				found = true;
			}
			_mutexSlave->unlock();
		}
	}

	return found;
}
