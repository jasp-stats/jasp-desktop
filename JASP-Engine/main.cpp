
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

    e->run();
}
