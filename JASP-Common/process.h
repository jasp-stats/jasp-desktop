#ifndef PROCESS_H
#define PROCESS_H

class Process
{
public:

	static unsigned long currentPID();
	static unsigned long parentPID();

	static bool isParentRunning();

};

#endif // PROCESS_H
