#ifndef PROCESSHELPER_H
#define PROCESSHELPER_H

#include <QProcessEnvironment>

///
/// Makes sure the relevant environment variables required for an engine to function are set in a central place.
///
class ProcessHelper
{
public:
	
    static QProcessEnvironment getProcessEnvironmentForJaspEngine(bool forceLC_CTYPE_C = false);
	
private:
	ProcessHelper(){}
};

#endif // PROCESSHELPER_H
