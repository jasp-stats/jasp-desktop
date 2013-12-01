#ifndef IPCCHANNEL_H
#define IPCCHANNEL_H

#ifdef __APPLE__
#include <semaphore.h>
#else
#include <boost/interprocess/sync/named_semaphore.hpp>
#endif

#include <boost/interprocess/sync/interprocess_mutex.hpp>

#include <boost/interprocess/managed_shared_memory.hpp>
#include <boost/container/string.hpp>

typedef boost::interprocess::allocator<char, boost::interprocess::managed_shared_memory::segment_manager> CharAllocator;
typedef boost::container::basic_string<char, std::char_traits<char>, CharAllocator> String;
typedef boost::interprocess::allocator<String, boost::interprocess::managed_shared_memory::segment_manager> StringAllocator;

class IPCChannel
{
public:
	IPCChannel(std::string name, int channelNumber, bool isSlave = false);

	void send(std::string &data);
	bool receive(std::string &data, int timeout = 0);

private:

	bool tryWait(int timeout = 0);

	std::string _name;
	int _channelNumber;
	bool _isSlave;

	boost::interprocess::managed_shared_memory *_memory;

	boost::interprocess::interprocess_mutex *_mutexOut;
	boost::interprocess::interprocess_mutex *_mutexIn;

	String *_dataOut;
	String *_dataIn;

#ifdef __APPLE__
	sem_t* _semaphoreOut;
	sem_t* _semaphoreIn;
#else
	boost::interprocess::named_semaphore* _semaphoreOut;
	boost::interprocess::named_semaphore* _semaphoreIn;
#endif



};

#endif // IPCCHANNEL_H
