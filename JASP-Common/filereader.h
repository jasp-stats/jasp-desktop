//
// Copyright (C) 2013-2016 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

#ifndef FILEREADER_H
#define FILEREADER_H

#include <string>
#include <vector>

#include <stdlib.h>
#include <boost/nowide/fstream.hpp>

#include "libzip/archive.h"

class FileReader
{
public:
	FileReader(const std::string &archivePath, const std::string &entryPath);
	FileReader(const std::string &path);
	~FileReader();

	int size() const;
	int pos() const;
	int bytesAvailable() const;
	bool isSequential() const;
	int readData(char * data, int maxSize, int &errorCode);
	char* readAllData(int blockSize, int &errorCode);
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
