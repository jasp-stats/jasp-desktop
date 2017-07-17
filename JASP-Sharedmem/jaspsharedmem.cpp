#include "jaspsharedmem.h"
#include <iostream>
#include <fstream>
using namespace std;


extern "C" JASPChannelInterface* CreateJASPChannel(const char* name, int slaveNo)
{
	std::cout << "Create JASP Channel Factory" << name << std::endl;
	std::cout.flush();
	return new JASPChannelImpl(name, slaveNo);
}

JASPChannelImpl::JASPChannelImpl(const char *name, int slaveNo)
{
	std::cout << "Create JASP Channel Implementation" << name << std::endl;
	std::cout.flush();
	_channel = new IPCChannel(name, slaveNo, true);
}

JASPChannelImpl::~JASPChannelImpl()
{

}

void JASPChannelImpl::destroy()
{

}

void JASPChannelImpl::send(const char *data)
{
	std::cout << "Write JASPChannelImpl " << data << std::endl;
	std::cout.flush();
	std::string strData(data);
	_channel->send(strData);
}

bool JASPChannelImpl::receive(char** data, int timeout)
{
	std::cout << "Read shared memory" << std::endl;
	std::cout.flush();
	std::string strData;
	timeout = 1000;
	bool result = _channel->receive(strData, timeout);
	if (result)
	{
		*data = new char[strData.size() + 1];
		std::copy(strData.begin(), strData.end(), *data);
		(*data)[strData.size()] = '\0';
	}

	return result;
}
