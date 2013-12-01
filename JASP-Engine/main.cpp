
#include "engine.h"

using namespace std;

int main(int argc, char *argv[])
{
    Engine *e = new Engine();

	if (argc > 1)
	{
		unsigned long slaveNo = 0;
		slaveNo = strtoul(argv[1], NULL, 10);
		e->setSlaveNo(slaveNo);
	}

#ifdef __WIN32__
	if (argc > 2)
    {
        unsigned long parentId = 0;
		parentId = strtoul(argv[2], NULL, 10);
        e->setParentPID(parentId);
    }
#endif

    e->run();
}
