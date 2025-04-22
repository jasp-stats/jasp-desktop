#include "timers.h"

#ifdef PROFILE_JASP
#include <algorithm>
#include <iostream>
#include <sstream>
#include <vector>
#include <thread>
#include <mutex>
static std::map<std::string, boost::timer::cpu_timer *> * timers = nullptr;



boost::timer::cpu_timer * _getTimer(std::string timerName)
{
	static std::mutex dontClobberYourself;
	
	//Log::log() << "getTimer! "<< timerName << std::endl;
	std::stringstream timerNameStream;
	timerNameStream << timerName << "_" << std::this_thread::get_id();
	timerName = timerNameStream.str();

	if(timers == nullptr)
	{
		dontClobberYourself.lock();
		if(timers == nullptr)
			timers = new std::map<std::string, boost::timer::cpu_timer *>();
		dontClobberYourself.unlock();
	}

	if(timers->count(timerName) == 0)
	{
		dontClobberYourself.lock();
		if(timers->count(timerName) == 0)
			(*timers)[timerName] = new boost::timer::cpu_timer(); //starts automatically
		dontClobberYourself.unlock();
	}
	
	return timers->at(timerName);
}

boost::timer::cpu_timer * _getTimerC(std::string timerName)
{
	std::stringstream timerNameStream;
	timerNameStream << timerName << "_" << std::this_thread::get_id();
	timerName = timerNameStream.str();

	return timers->at(timerName);
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
		std::cerr << keyval.first << " ran for " << keyval.second->format() << std::endl;
}

#endif
