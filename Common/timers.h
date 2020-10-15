#ifndef TIMERS_H
#define TIMERS_H

#ifdef PROFILE_JASP

#include <boost/timer/timer.hpp>
#include <string>
#include <map>


boost::timer::cpu_timer * _getTimer(std::string timerName);
void _printAllTimers();

#define JASPTIMER_START(  TIMERNAME ) _getTimer( #TIMERNAME )->start()
#define JASPTIMER_RESUME( TIMERNAME ) _getTimer( #TIMERNAME )->resume()
#define JASPTIMER_STOP(   TIMERNAME ) _getTimer( #TIMERNAME )->stop()
#define JASPTIMER_PRINT(  TIMERNAME ) Log::log() << #TIMERNAME << " ran for " << _getTimer( #TIMERNAME )->format() << std::endl
#define JASPTIMER_FINISH( TIMERNAME ) JASPTIMER_STOP(TIMERNAME); JASPTIMER_PRINT(TIMERNAME)
#define JASPTIMER_PRINTALL() _printAllTimers()

struct _JaspTimerScopeMeasure
{
	_JaspTimerScopeMeasure(const char * name) : _name(name) { _getTimer(_name)->resume(); }
	~_JaspTimerScopeMeasure()								{ _getTimer(_name)->stop(); }

	const char * _name;
};
#define JASPTIMER_SCOPE(TIMERNAME) _JaspTimerScopeMeasure singleScopeTimer(#TIMERNAME)
#define JASPTIMER_CLASS(TIMERNAME) _JaspTimerScopeMeasure singleScopeTimer = #TIMERNAME;

#else
//No timers please!
#define JASPTIMER_START(  TIMERNAME ) /* TIMERNAME */
#define JASPTIMER_RESUME( TIMERNAME ) /* TIMERNAME */
#define JASPTIMER_STOP(   TIMERNAME ) /* TIMERNAME */
#define JASPTIMER_PRINT(  TIMERNAME ) /* TIMERNAME */
#define JASPTIMER_FINISH( TIMERNAME ) /* TIMERNAME */
#define JASPTIMER_PRINTALL() /* bla bla bla */
#define JASPTIMER_SCOPE(TIMERNAME) /* Hmm hmm */
#define JASPTIMER_CLASS(TIMERNAME) /* Hmm hmm */
#endif

#endif // TIMERS_H
