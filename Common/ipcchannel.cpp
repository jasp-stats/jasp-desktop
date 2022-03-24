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

#include "ipcchannel.h"
#include "tempfiles.h"

#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/nowide/convert.hpp>
#include "log.h"

using namespace std;
using namespace boost;
using namespace boost::posix_time;

IPCChannel::IPCChannel(std::string name, size_t channelNumber, bool isSlave)
	:
	  _baseName(		name + "_" + std::to_string(channelNumber)	),
	  _nameControl(		name + "_control"							),
	  _nameMtS(			name + "_MasterToSlave"						),
	  _nameStM(			name + "_SlaveToMaster"						),
	  _channelNumber(	channelNumber								),
	  _isSlave(			isSlave										)
{
	_memoryControl			= new interprocess::managed_shared_memory(interprocess::open_or_create, _baseName.c_str(), 4096);

	_sizeMtoS				= _memoryControl->find_or_construct<size_t>("sizeMasterToSlave")(1024 * 1024 * 8);
	_sizeStoM				= _memoryControl->find_or_construct<size_t>("sizeSlaveToMaster")(1024 * 1024 * 8);

	_memoryMasterToSlave	= new interprocess::managed_shared_memory(interprocess::open_or_create, _nameMtS.c_str(), *_sizeMtoS);
	_memorySlaveToMaster	= new interprocess::managed_shared_memory(interprocess::open_or_create, _nameStM.c_str(), *_sizeStoM);

	TempFiles::addShmemFileName(_baseName);
	TempFiles::addShmemFileName(_nameMtS);
	TempFiles::addShmemFileName(_nameStM);

	generateNames();

	_mutexIn  = _memoryControl->find_or_construct<interprocess::interprocess_mutex>(_mutexInName.c_str())();
	_mutexOut = _memoryControl->find_or_construct<interprocess::interprocess_mutex>(_mutexOutName.c_str())();

	_memoryIn  = _isSlave ? _memoryMasterToSlave : _memorySlaveToMaster;
	_memoryOut = _isSlave ? _memorySlaveToMaster : _memoryMasterToSlave;

	_sizeIn  = _isSlave ? _sizeMtoS : _sizeStoM;
	_sizeOut = _isSlave ? _sizeStoM : _sizeMtoS;

	_previousSizeIn = *_sizeIn;
	_previousSizeOut = *_sizeOut;

	_dataIn		= _memoryIn->find_or_construct<String>(_dataInName.c_str())		(_memoryIn->get_segment_manager());
	_dataOut	= _memoryOut->find_or_construct<String>(_dataOutName.c_str())	(_memoryOut->get_segment_manager());

#ifdef __APPLE__
	_semaphoreIn  = sem_open(_mutexInName.c_str(),  O_CREAT, S_IWUSR | S_IRGRP | S_IROTH, 0);
	_semaphoreOut = sem_open(_mutexOutName.c_str(), O_CREAT, S_IWUSR | S_IRGRP | S_IROTH, 0);

	if (isSlave == false)
	{
		// cleanse the semaphores; they don't seem to reliably initalise to zero.

		while (sem_trywait(_semaphoreIn) == 0) ; // do nothing
		while (sem_trywait(_semaphoreOut) == 0); // do nothing

	}
#elif defined _WIN32

	wstring inName  = nowide::widen(_semaphoreInName);
	wstring outName = nowide::widen(_semaphoreOutName);

	LPCWSTR inLPCWSTR  = inName.c_str();
	LPCWSTR outLPCWSTR = outName.c_str();

	if (isSlave == false)
	{
		_semaphoreIn  = CreateSemaphore(NULL, 0, 1, inLPCWSTR);
		_semaphoreOut = CreateSemaphore(NULL, 0, 1, outLPCWSTR);
	}
	else
	{
		_semaphoreIn  = OpenSemaphore(SYNCHRONIZE,							false, inLPCWSTR);
		_semaphoreOut = OpenSemaphore(SYNCHRONIZE | SEMAPHORE_MODIFY_STATE,	false, outLPCWSTR);
	}

#else

	if (_isSlave == false)
	{
		interprocess::named_semaphore::remove(_mutexInName.c_str());
		interprocess::named_semaphore::remove(_mutexOutName.c_str());

		_semaphoreIn  = new interprocess::named_semaphore(interprocess::create_only, _mutexInName.c_str(), 0);
		_semaphoreOut = new interprocess::named_semaphore(interprocess::create_only, _mutexOutName.c_str(), 0);
	}
	else
	{
		_semaphoreIn  = new interprocess::named_semaphore(interprocess::open_only, _mutexInName.c_str());
		_semaphoreOut = new interprocess::named_semaphore(interprocess::open_only, _mutexOutName.c_str());
	}


#endif
}

IPCChannel::~IPCChannel()
{
#ifdef JASP_DEBUG
	Log::log() << "~IPCChannel() of " << (_isSlave ? "Slave" : "Master") << std::endl;
#endif
	if(_isSlave)
		return;

	delete _memoryControl;
	delete _memoryMasterToSlave;
	delete _memorySlaveToMaster;

	_memoryControl			= nullptr;
	_memoryMasterToSlave	= nullptr;
	_memorySlaveToMaster	= nullptr;

	interprocess::shared_memory_object::remove(_baseName.c_str());
	interprocess::shared_memory_object::remove(_nameMtS.c_str());
	interprocess::shared_memory_object::remove(_nameStM.c_str());
}

void IPCChannel::generateNames()
{
	stringstream mutexInName, mutexOutName, dataInName, dataOutName, semaphoreInName, semaphoreOutName;

	std::string in  = _isSlave ? "-s" : "-m";
	std::string out = _isSlave ? "-m" : "-s";

	dataInName			<< _baseName << in  << 'd' << _channelNumber;
	dataOutName			<< _baseName << out << 'd' << _channelNumber;
	mutexInName			<< _baseName << in  << 'm' << _channelNumber;
	mutexOutName		<< _baseName << out << 'm' << _channelNumber;
	semaphoreInName		<< _baseName << in  << 's' << _channelNumber;
	semaphoreOutName	<< _baseName << out << 's' << _channelNumber;

	_semaphoreOutName	= semaphoreOutName.str();
	_semaphoreInName	= semaphoreInName.str();
	_mutexOutName		= mutexOutName.str();
	_mutexInName		= mutexInName.str();
	_dataOutName		= dataOutName.str();
	_dataInName			= dataInName.str();
}

void IPCChannel::rebindMemoryInIfSizeChanged()
{
	if(_previousSizeIn < *_sizeIn)
	{
		Log::log() << "rebindMemoryInIfSizeChanged! Size changed!\n" << std::flush;

		delete _memoryIn;
		_previousSizeIn = *_sizeIn;
		_memoryIn		= new interprocess::managed_shared_memory(interprocess::open_only, _isSlave ? _nameMtS.c_str() : _nameStM.c_str());

		if(_isSlave)	_memoryMasterToSlave	= _memoryIn;
		else			_memorySlaveToMaster	= _memoryIn;

		_dataIn = _memoryIn->find<String>(_dataInName.c_str()).first;
	}
}

void IPCChannel::doubleMemoryOut()
{
	Log::log() << "IPCChannel::doubleMemoryOut is called and new memsize: ";

	std::string memOutName = _isSlave ? _nameStM : _nameMtS;

	_memoryOut->destroy<String>(_dataOutName.c_str());

	if(!interprocess::managed_shared_memory::grow(memOutName.c_str(), *_sizeOut))
		throw std::runtime_error("Growing IPCChannel failed!");

	_memoryOut = new interprocess::managed_shared_memory(interprocess::open_only, memOutName.c_str());

	if(_isSlave)	_memorySlaveToMaster = _memoryOut;
	else			_memoryMasterToSlave = _memoryOut;

	_dataOut	= _memoryOut->construct<String>(_dataOutName.c_str())(_memoryOut->get_segment_manager());
	*_sizeOut	= _memoryOut->get_size();

	Log::log() << *_sizeOut << "\n" << std::flush;
}

void IPCChannel::send(string &&data, bool alreadyLockedMutex)
{
	send(data, alreadyLockedMutex);
}


void IPCChannel::send(string &data, bool alreadyLockedMutex)
{
	if(!alreadyLockedMutex)
		_mutexOut->lock();

	try
	{
		_dataOut->assign(data.begin(), data.end());
	}
	catch (boost::interprocess::bad_alloc &e)	{ goto retryAfterDoublingMemory; }
	catch (std::length_error &e)				{ goto retryAfterDoublingMemory; }
	catch(std::exception & e)
	{
		Log::log() << "IPCChannel::send encountered an exception: " << e.what() << std::endl;
		throw e; //no need to unlock because this will crash stuff
	}

#ifdef __APPLE__
	sem_post(_semaphoreOut);
#elif defined _WIN32
	ReleaseSemaphore(_semaphoreOut, 1, NULL);
#else
	_semaphoreOut->post();
#endif


	_mutexOut->unlock();
	return; // return here to avoid going to retryAfterDoublingMemory

retryAfterDoublingMemory:
		Log::log() << "IPCChannel::send out buffer is too small!\n" << std::flush;

		doubleMemoryOut();

		send(data, true); //try again!
}

bool IPCChannel::receive(string &data, int timeout)
{

	if (tryWait(timeout))
	{
		_mutexIn->lock();

		while (tryWait()); // clear it completely

		try
		{
			rebindMemoryInIfSizeChanged();
			data.assign(_dataIn->c_str(), _dataIn->size());
		}
		catch(std::exception & e)
		{
			Log::log() << "IPCChannel::receive encountered an exception: " << e.what() << std::endl;
			throw e;
		}

		_mutexIn->unlock();

		return true;
	}

	return false;
}


bool IPCChannel::tryWait(int timeout)
{
	bool messageWaiting;

#ifdef __APPLE__

	messageWaiting = sem_trywait(_semaphoreIn) == 0;

	while (timeout > 0 && messageWaiting == false)
	{
		usleep(100000);
		timeout -= 10;
		messageWaiting = sem_trywait(_semaphoreIn) == 0;
	}

#elif defined _WIN32

	messageWaiting = (WaitForSingleObject(_semaphoreIn, timeout) == WAIT_OBJECT_0);

#else

	if (timeout > 0)
	{
		ptime now(microsec_clock::universal_time());
		ptime then = now + microseconds(1000 * timeout);

		messageWaiting = _semaphoreIn->timed_wait(then);
	}
	else
	{
		messageWaiting = _semaphoreIn->try_wait();
	}
#endif

	return messageWaiting;

}

std::string IPCChannel::lastSentMsg() const
{
	return _dataOut->data();
}
