#ifndef OTOOLSTUFF_H
#define OTOOLSTUFF_H

#include <string>

/// These functions are used on MacOS to bend the rpath in R-library-binaries to our bundled R.framework
/// Depends on otool being present and callable in the users system.

std::string _system(					std::string		cmd);
void		_moduleLibraryFixer(const	std::string &	moduleLibrary, bool engineCall = false, bool printStuff = false);

#endif // OTOOLSTUFF_H
