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

#ifndef IPCCHANNEL_H
#define IPCCHANNEL_H

/* for reasons unclear to me, the boost semaphores use 100% CPU on OS X
 * and sometimes under windows too. hence, there are platform specific
 * implementations below */

#ifdef  _WIN32
#undef Realloc
#undef Free
#include <windows.h>
#endif

#include <boost/interprocess/managed_shared_memory.hpp>
#include <boost/container/string.hpp>
#include <functional>
#include <thread>

typedef boost::interprocess::allocator<char,	boost::interprocess::managed_shared_memory::segment_manager	> CharAllocator;
typedef boost::container::basic_string<char,	std::char_traits<char>, CharAllocator						> String;
typedef boost::interprocess::allocator<String,	boost::interprocess::managed_shared_memory::segment_manager	> StringAllocator;

///
/// IPCChannel or Interproces communication channel
/// Roughly a string guarded by a mutex to have a one way communication channel between Engine and Desktop
/// This means that two of these are needed to have, well you guessed it, two way communication.
/// It is created with a certain size but if it needs to grow (because of massive messages) it will double in size until it accomodates the message.
///
class IPCChannel
{
public:
	IPCChannel(std::string name, size_t channelNumber, bool isSlave = false);
	~IPCChannel();

	std::string lastSentMsg() const;

	void send(std::string		&	data,	bool alreadyLockedMutex = false);
	void send(std::string		&&	data,	bool alreadyLockedMutex = false);
	bool receive(std::string	&	data,	int timeout = 0);

	size_t channelNumber() { return _channelNumber; }

	void findConstructAllAgain();

	bool jaspAlive();

private:
	bool tryWait(int timeout = 0);
	void catchAndRepeat(const std::string & taskDescription, std::function<void()> doThis);

	void doubleMemoryOut();
	void rebindMemoryInIfSizeChanged();
	void generateNames();

	void findConstructSizes();
	void findConstructDataStrings();
	void findConstructMutexes();

	static bool										heartbeat(std::string path, unsigned int delayMs);
	static std::thread								_heartbeatThread;

	int64_t											_lastHeartBeatTimestamp = 0;
	std::string										_jaspHeartBeatPath;
	unsigned int									_heatbeatDelayS = 5;
	unsigned int									_maxHeartbeatDiffS = 100;

	std::string										_baseName,
													_nameControl,
													_nameMtS,
													_nameStM;
	size_t											_channelNumber;
	bool											_isSlave;
	boost::interprocess::managed_shared_memory	*	_memoryControl			= nullptr,
												*	_memoryMasterToSlave	= nullptr,
												*	_memorySlaveToMaster	= nullptr,
												*	_memoryIn				= nullptr,
												*	_memoryOut				= nullptr;
	boost::interprocess::interprocess_mutex		*	_mutexOut				= nullptr,
												*	_mutexIn				= nullptr;
	String										*	_dataOut				= nullptr,
												*	_dataIn					= nullptr;
	size_t										*	_sizeMtoS				= nullptr,
												*	_sizeStoM				= nullptr,
												*	_sizeIn					= nullptr,
												*	_sizeOut				= nullptr,
													_previousSizeIn,
													_previousSizeOut;
	std::string										_mutexInName,
													_mutexOutName,
													_dataInName,
													_dataOutName,
													_semaphoreInName,
													_semaphoreOutName;
	uint64_t										_msgIDSend				= 0,
													_msgIDRecv				= 1;
};

#endif // IPCCHANNEL_H
