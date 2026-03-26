#ifndef TIMERS_H
#define TIMERS_H

#ifdef PROFILE_JASP

///
/// This file contains some simple timers that can be added to a variety of locations in JASP to be able to profile easily
/// To do so PROFILE_JASP can be defined in the build-environment and then rebuilt.
/// If it isn't used it just compiles into some comments and thus thrown out entirely by the preprocessor.

#include <chrono>
#include <string>
#include <map>

struct customtimer
{
	
	
	void	start()
	{
			lastStart = std::chrono::steady_clock::now();
			totalStarts++;
	}
	
	void resume() { start(); }
	
	void	stop()
	{
		const std::chrono::duration<double> diff = std::chrono::steady_clock::now() - lastStart;
		totalDuration += diff.count();
	}
	
	std::string format()
	{
		return std::to_string(totalDuration) + "s in " + std::to_string(totalStarts) + " times for an average of "+ std::to_string(totalDuration / std::max(totalStarts, 1))+"s per call";
	}
	
	std::chrono::time_point<std::chrono::steady_clock>	lastStart;
	double												totalDuration = 0;
	int													totalStarts = 0;
};


customtimer * _getTimer(std::string timerName);
customtimer * _getTimerC(std::string timerName);
void _printAllTimers();

#define JASPTIMER_STOP(     TIMERNAME ) _getTimerC(#TIMERNAME )->stop()
#define JASPTIMER_START(    TIMERNAME ) _getTimer( #TIMERNAME )->start()
#define JASPTIMER_STARTQML( TIMERNAME ) _getTimer(  TIMERNAME )->start()
#define JASPTIMER_STOPQML(  TIMERNAME ) _getTimerC( TIMERNAME )->stop()
#define JASPTIMER_RESUME(   TIMERNAME ) _getTimer( #TIMERNAME )->resume()
#define JASPTIMER_PRINT(    TIMERNAME ) Log::log() << #TIMERNAME << " ran for " << _getTimer( #TIMERNAME )->format() << std::endl
#define JASPTIMER_FINISH(   TIMERNAME ) JASPTIMER_STOP(TIMERNAME); JASPTIMER_PRINT(TIMERNAME)
#define JASPTIMER_PRINTALL() _printAllTimers()

struct _JaspTimerScopeMeasure
{
	_JaspTimerScopeMeasure(const char * name) : _name(name) { try{ _getTimer(_name)->resume();	} catch(...) {} }
	~_JaspTimerScopeMeasure()								{ try{ _getTimerC(_name)->stop();	} catch(...) {} }

	const char * _name;
};
#define JASPTIMER_SCOPE(TIMERNAME) _JaspTimerScopeMeasure singleScopeTimer(#TIMERNAME)
#define JASPTIMER_CLASS(TIMERNAME) _JaspTimerScopeMeasure singleScopeTimer = #TIMERNAME;

#else
//No timers please!
#define JASPTIMER_STOP(   TIMERNAME ) /* TIMERNAME */
#define JASPTIMER_START(  TIMERNAME ) /* TIMERNAME */
#define JASPTIMER_STOPQML(   TIMERNAME ) /* TIMERNAME */
#define JASPTIMER_STARTQML(  TIMERNAME ) /* TIMERNAME */
#define JASPTIMER_RESUME( TIMERNAME ) /* TIMERNAME */
#define JASPTIMER_PRINT(  TIMERNAME ) /* TIMERNAME */
#define JASPTIMER_FINISH( TIMERNAME ) /* TIMERNAME */
#define JASPTIMER_PRINTALL() /* bla bla bla */
#define JASPTIMER_SCOPE(TIMERNAME) /* Hmm hmm */
#define JASPTIMER_CLASS(TIMERNAME) /* Hmm hmm */
#endif

#endif // TIMERS_H
