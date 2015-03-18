#ifndef DIRS_H
#define DIRS_H

#include <string>
#include <stdexcept>

class Dirs
{
public:

	static std::string appDataDir();
	static std::string tempDir();
	static std::string exeDir();
	static std::string rHomeDir();
	static std::string libraryDir();

	class Exception : public std::runtime_error
	{
	public:
		Exception(const std::string &message, std::runtime_error &)
			: runtime_error(message.c_str()) { }

		Exception(const std::string &message)
			: runtime_error(message.c_str()) { }
	};

};

#endif // DIRS_H
