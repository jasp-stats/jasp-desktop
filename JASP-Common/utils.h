#ifndef UTILS_H
#define UTILS_H

#include <string>
#include <vector>
#include <boost/filesystem.hpp>

class Utils
{
public:

	static long currentMillis();
	static long currentSeconds();
	static long getFileModificationTime(const std::string &filename);
	static long getFileSize(const std::string &filename);
	static void touch(const std::string &filename);
	static bool renameAtomic(const std::string &oldName, const std::string &newName);

	static boost::filesystem::path osPath(const std::string &path);
	static std::string osPath(const boost::filesystem::path &path);

	static void remove(std::vector<std::string> &target, const std::vector<std::string> &toRemove);
};

#endif // UTILS_H
