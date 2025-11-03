#include "timers.h"

#ifdef PROFILE_JASP
#include <algorithm>
#include <iostream>
#include <sstream>
#include <vector>
#include <thread>
#include <mutex>
#include "log.h"
static std::map<std::string, customtimer *> * timers = nullptr;



customtimer * _getTimer(std::string timerName)
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
			timers = new std::map<std::string, customtimer *>();
		dontClobberYourself.unlock();
	}

	if(timers->count(timerName) == 0)
	{
		dontClobberYourself.lock();
		if(timers->count(timerName) == 0)
			(*timers)[timerName] = new customtimer(); //starts automatically
		dontClobberYourself.unlock();
	}
	
	return timers->at(timerName);
}

customtimer * _getTimerC(std::string timerName)
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
	
	typedef std::pair<std::string, customtimer *> nameTimerPair;
	
	std::vector<nameTimerPair> sortMe(timers->begin(), timers->end());
	
	std::sort(sortMe.begin(), sortMe.end(), [](const nameTimerPair & l, const nameTimerPair & r)
	{
		return l.second->totalDuration > r.second->totalDuration;
	});

	for(const nameTimerPair & keyval : sortMe)
		Log::log() << keyval.first << " ran for " << keyval.second->format() << std::endl;
}

#endif
