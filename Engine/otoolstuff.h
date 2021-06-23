#ifndef OTOOLSTUFF_H
#define OTOOLSTUFF_H

#include <string>

std::string _system(					std::string		cmd);
void		_moduleLibraryFixer(const	std::string &	moduleLibrary, bool engineCall = false, bool useLogger = true, bool printStuff = false);

#endif // OTOOLSTUFF_H
