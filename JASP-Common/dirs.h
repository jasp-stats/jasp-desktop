#ifndef DIRS_H
#define DIRS_H

#include <string>

class Dirs
{
public:

	static std::string appDataDir();
	static std::string tempDir();
	static std::string exeDir();
	static std::string rHomeDir();

};

#endif // DIRS_H
