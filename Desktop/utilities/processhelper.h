#ifndef PROCESSHELPER_H
#define PROCESSHELPER_H

#include <QProcessEnvironment>

class ProcessHelper
{
public:
	
	static QProcessEnvironment getProcessEnvironmentForJaspEngine(bool withTmpDir, bool forceLC_CTYPE_C = false);
	
private:
	ProcessHelper(){}
};

#endif // PROCESSHELPER_H
