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
#include "utils.h"

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
	Log::log() << "IPCChannel(" << name << ", " << channelNumber << ", " << (isSlave ? "slave" : "master") << ");" << std::endl;

	Log::log() << (!_isSlave ? "Creating control memory" : "Opening control memory") << std::endl;

	_memoryControl			= !isSlave ? new interprocess::managed_shared_memory(interprocess::open_or_create,	_baseName.c_str(), 4096)
									   : new interprocess::managed_shared_memory(interprocess::open_only,		_baseName.c_str());

	if(!_isSlave)
	{
		_sizeMtoS				= _memoryControl->find_or_construct<size_t>("sizeMasterToSlave")(1024 * 1024 * 8);
		_sizeStoM				= _memoryControl->find_or_construct<size_t>("sizeSlaveToMaster")(1024 * 1024 * 8);
	}
	else
		catchAndRepeat("Finding sizes if communication channels", [&]()
		{
			auto foundSizeMtoS  = _memoryControl->find<size_t>("sizeMasterToSlave");
			auto foundSizeStoM  = _memoryControl->find<size_t>("sizeSlaveToMaster");

			if(foundSizeMtoS.first == nullptr)	throw std::runtime_error("Couldn't find sizeMasterToSlave for IPCChannel...");
			if(foundSizeStoM.first == nullptr)	throw std::runtime_error("Couldn't find sizeSlaveToMaster for IPCChannel...");

			_sizeMtoS = foundSizeMtoS.first;  //It could actually be an array, but of course, this would just take the first one and we ignore the rest.
			_sizeStoM = foundSizeStoM.first;

			if(foundSizeMtoS.second > 1 || foundSizeStoM.second > 1)
				Log::log() << "More than 1 (_sizeMtoS: " << foundSizeMtoS.second << " _sizeStoM: " << foundSizeStoM.second << ") data String found in IPCChannel startup on engine." << std::endl;
		});

	if(!_isSlave)
	{
		Log::log() << "Creating communication memory" << std::endl;
		_memoryMasterToSlave	= new interprocess::managed_shared_memory(interprocess::open_or_create, _nameMtS.c_str(), *_sizeMtoS);
		_memorySlaveToMaster	= new interprocess::managed_shared_memory(interprocess::open_or_create, _nameStM.c_str(), *_sizeStoM);
	}
	else
		catchAndRepeat("Opening communications memory", [&]()
		{

			Log::log() << "Opening M->S" << std::endl;
			_memoryMasterToSlave	= new interprocess::managed_shared_memory(interprocess::open_only, _nameMtS.c_str());
			Log::log() << "Opening S->M" << std::endl;
			_memorySlaveToMaster	= new interprocess::managed_shared_memory(interprocess::open_only, _nameStM.c_str());
		});


	generateNames();

	Log::log() << "Finding/constructing mutexes" << std::endl;

	if(!_isSlave)	findConstructMutexes();
	else
		catchAndRepeat("Finding mutexes", [&]()
		{
			auto mutexesIn  = _memoryControl->find<interprocess::interprocess_mutex>(_mutexInName.c_str());
			auto mutexesOut = _memoryControl->find<interprocess::interprocess_mutex>(_mutexOutName.c_str());

			if(mutexesIn.first  == nullptr)	throw std::runtime_error("Couldn't find mutex in for IPCChannel...");
			if(mutexesOut.first == nullptr)	throw std::runtime_error("Couldn't find mutex out for IPCChannel...");

			_mutexIn	= mutexesIn.first;
			_mutexOut	= mutexesOut.first;
		});

	_memoryIn	= _isSlave ? _memoryMasterToSlave : _memorySlaveToMaster;
	_memoryOut	= _isSlave ? _memorySlaveToMaster : _memoryMasterToSlave;

	_sizeIn		= _isSlave ? _sizeMtoS : _sizeStoM;
	_sizeOut	= _isSlave ? _sizeStoM : _sizeMtoS;

	_previousSizeIn  = *_sizeIn;
	_previousSizeOut = *_sizeOut;

	if(!_isSlave)
		findConstructDataStrings();
	else
		catchAndRepeat("Finding communication strings", [&]()
		{
			Log::log() << "Opening " << _dataInName << std::endl;
			auto foundDataIn  = _memoryIn ->find<String>(_dataInName.c_str());

			Log::log() << "Opening " << _dataOutName << std::endl;
			auto foundDataOut = _memoryOut->find<String>(_dataOutName.c_str());

			if(foundDataIn.first == nullptr)	throw std::runtime_error("Couldn't find data in for IPCChannel...");
			if(foundDataOut.first == nullptr)	throw std::runtime_error("Couldn't find data out for IPCChannel...");

			_dataIn	 = foundDataIn.first;  //It could actually be an array, but of course, this would just take the first one and we ignore the rest.
			_dataOut = foundDataOut.first;

			if(foundDataIn.second > 1 || foundDataOut.second > 1)
				Log::log() << "More than 1 (in: " << foundDataIn.second << " out: " << foundDataOut.second << ") data String found in IPCChannel startup on engine." << std::endl;
		});

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

	//Log::log() << "IPCChannel init done" << std::endl;
}

void IPCChannel::findConstructSizes()
{
	_sizeMtoS	= _memoryControl->find_or_construct<size_t>("sizeMasterToSlave")(1024 * 1024 * 8);
	_sizeStoM	= _memoryControl->find_or_construct<size_t>("sizeSlaveToMaster")(1024 * 1024 * 8);
}


void IPCChannel::findConstructMutexes()
{
	_mutexIn  = _memoryControl->find_or_construct<interprocess::interprocess_mutex>(_mutexInName.c_str())();
	_mutexOut = _memoryControl->find_or_construct<interprocess::interprocess_mutex>(_mutexOutName.c_str())();
}


void IPCChannel::findConstructDataStrings()
{
	Log::log() << "Finding/constructing communication strings" << std::endl;

	Log::log() << "Creating " << _dataInName << std::endl;
	_dataIn		= _memoryIn ->find_or_construct<String>(_dataInName.c_str())	(_memoryIn ->get_segment_manager());

	Log::log() << "Creating " << _dataOutName << std::endl;
	_dataOut	= _memoryOut->find_or_construct<String>(_dataOutName.c_str())	(_memoryOut->get_segment_manager());
}

void IPCChannel::findConstructAllAgain()
{
	Log::log() << "Finding/constructing all relevant shared memory objects again." << std::endl;
	findConstructSizes();
	findConstructMutexes();
	findConstructDataStrings();
}

void IPCChannel::catchAndRepeat(const std::string & taskDescription, std::function<void()> doThis)
{
	const long	now		= Utils::currentMillis(),
				wait	= 2000,
				sleep	= 200;

	bool worked = false;

	Log::log() << taskDescription << " for at least " << wait << "ms" << std::endl;


	while(!worked && Utils::currentMillis() < now + wait)
		try
		{
			doThis();

			worked = true;
		}
		catch(std::exception e)
		{
			Log::log() << "Had exception: " << e.what() << std::endl;
			Log::log() << "Will sleep for " << sleep << "ms and try again." << std::endl;

			Utils::sleep(sleep);
		}

	if(!worked)
	{
		static std::string failure = taskDescription + " failed...";
		Log::log() << failure << std::endl;
		throw std::runtime_error(failure);
	}
}

IPCChannel::~IPCChannel()
{

#ifdef JASP_DEBUG
	Log::log() << "~IPCChannel(#" << _channelNumber << ") of " << (_isSlave ? "Slave" : "Master") << std::endl;
#endif

	delete _memoryControl;
	delete _memoryMasterToSlave;
	delete _memorySlaveToMaster;

	_memoryControl			= nullptr;
	_memoryMasterToSlave	= nullptr;
	_memorySlaveToMaster	= nullptr;

	if(_isSlave)
		return;

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
