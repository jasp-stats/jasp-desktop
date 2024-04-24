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
	
	typedef std::pair<std::string, boost::timer::cpu_timer *> nameTimerPair;
	
	std::vector<nameTimerPair> sortMe(timers->begin(), timers->end());
	
	std::sort(sortMe.begin(), sortMe.end(), [](const nameTimerPair & l, const nameTimerPair & r)
	{
		return l.second->elapsed().user > r.second->elapsed().user;
	});

	for(const nameTimerPair & keyval : sortMe)
		std::cout << keyval.first << " ran for " << keyval.second->format() << std::endl;
}

#endif
