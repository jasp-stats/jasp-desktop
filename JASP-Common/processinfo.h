#ifndef PROCESSINFO_H
#define PROCESSINFO_H

class ProcessInfo
{
public:

	static unsigned long currentPID();
	static unsigned long parentPID();

	static bool isParentRunning();

};

#endif // PROCESS_H
