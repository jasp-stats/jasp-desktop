#ifndef FILEREADER_H
#define FILEREADER_H

#include <string>
#include <boost/nowide/fstream.hpp>
#include "libzip/archive.h"

class FileReader
{
public:
	FileReader(const std::string &archivePath, const std::string &entryPath);
	FileReader(const std::string &path);
	~FileReader();

	int size() const;
	int bytesAvailable() const;
	bool isSequential() const;
	int readData(char * data, int maxSize);
	void close();
	void reset();
	bool isClosed();
	bool exists() const;
	bool archiveExists() const;
	std::string fileName() const;
	std::string extension() const;

	static std::vector<std::string> getEntryPaths(const std::string &archivePath, const std::string &entryBaseDirectory = std::string());

private:

	struct archive *_archive;
	boost::nowide::ifstream *_file = NULL;

	bool _isArchive = false;

	int _size = 0;
	int _currentRead = 0;
	bool _isOpen = false;
	bool _exists = false;
	bool _archiveExists = false;
	std::string _archivePath;
	std::string _entryPath;

	void openEntry(const std::string &archivePath, const std::string &entryPath);
	void openFile(const std::string &filePath);
};

#endif // FILEREADER_H
