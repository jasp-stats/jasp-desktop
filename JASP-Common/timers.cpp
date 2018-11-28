#include "timers.h"

#ifdef PROFILE_JASP
#include  <iostream>

boost::timer::cpu_timer * _getTimer(std::string timerName)
{

	//std::cout << "getTimer! "<< timerName << std::endl;

	static auto * timers = new std::map<std::string, boost::timer::cpu_timer *>();

	if(timers->count(timerName) == 0)
	{
		(*timers)[timerName] = new boost::timer::cpu_timer(); //starts automatically
		(*timers)[timerName]->stop();
	}

	return (*timers)[timerName];
	return NULL;
}

#endif
