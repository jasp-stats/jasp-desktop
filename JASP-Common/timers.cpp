#include "timers.h"

#ifdef PROFILE_JASP
#include  <iostream>

static std::map<std::string, boost::timer::cpu_timer *> * timers = nullptr;

boost::timer::cpu_timer * _getTimer(std::string timerName)
{

	//Log::log() << "getTimer! "<< timerName << std::endl;

	if(timers == nullptr)
		timers = new std::map<std::string, boost::timer::cpu_timer *>();

	if(timers->count(timerName) == 0)
	{
		(*timers)[timerName] = new boost::timer::cpu_timer(); //starts automatically
		(*timers)[timerName]->stop();
	}

	return (*timers)[timerName];
}

void _printAllTimers()
{
	if(timers == nullptr)
		return;

	for(auto keyval : *timers)
		std::cout << keyval.first << " ran for " << keyval.second->format() << std::endl;
}

#endif
