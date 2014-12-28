
#include "dirs.h"

#include <iostream>
#include <libproc.h>

#include "process.h"


using namespace std;

string Dirs::exeDir()
{
#ifdef __WIN32__
	return "";
#else

	unsigned long pid = Process::currentPID();

	char pathbuf[PROC_PIDPATHINFO_MAXSIZE];
	int ret = proc_pidpath (pid, pathbuf, sizeof(pathbuf));

	if (ret <= 0)
	{
		cout << "Could not retrieve exe location\n";
		cout.flush();

		throw exception();
	}
	else
	{
		int last = strlen(pathbuf);

		for (int i = last - 1; i > 0; i--)
		{
			if (pathbuf[i] == '/')
			{
				pathbuf[i] = '\0';
				break;
			}
		}

		return string(pathbuf);
	}

#endif

}

string Dirs::rHomeDir()
{
#ifdef __APPLE__

	string dir = exeDir();
	dir += "/../Frameworks/R.framework/Versions/3.1/Resources";

	return dir;

#endif

	return "";
}

