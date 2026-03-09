#ifndef PROCESSHELPER_H
#define PROCESSHELPER_H

#include <QProcessEnvironment>

///
/// Makes sure the relevant environment variables required for an engine to function are set in a central place.
///
class ProcessHelper
{
public:
	
    static QProcessEnvironment	getProcessEnvironmentForJaspEngine();
#ifdef _WIN32 
	static void					fixPATHForWindows(QProcessEnvironment & env);
#endif
	
private:
	ProcessHelper(){}
};

#endif // PROCESSHELPER_H
