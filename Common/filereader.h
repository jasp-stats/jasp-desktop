//
// Copyright (C) 2013-2018 University of Amsterdam
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

/**
 * @brief The FileReader class - Reads archives or simple files.
 *
 * Reads (archive) files, and extracts entiries in the archive.
 *
 */
class FileReader
{
public:
	/**
	 * @brief FileReader Ctor to read an entry in the archieve.
	 * @param archivePath - Path to archive file.
	 * @param entryPath - Path to entry in archive,
	 *
	 */
	FileReader(const std::string &archivePath, const std::string &entryPath);

	/**
	 * @brief FileReader - Ctor to read simple file.
	 * @param path - Path to archive.
	 */
	FileReader(const std::string &path);

	/**
	 * @brief Dtor()
	 *
	 */
	~FileReader();

	/**
	 * @brief size Sizeof archive, or entry.
	 * @return size found.
	 */
	int size() const;

	/**
	 * @brief pos The current position in the file.
	 * @return Bytes from start of file.
	 */
	int pos() const;

	/**
	 * @brief bytes Available Bytes in the file or entry.
	 * @return Number bytes still to be read.
	 */
	int bytesAvailable() const;

	/**
	 * @brief isSequential Is file access sequnential.
	 * @return true.
	 */
	bool isSequential() const;

	/**
	 * @brief readData Reads at most maxSize bytes to data.
	 * @param data Output buffer.
	 * @param maxSize Maximum number bytes to read.
	 * @param errorCode On success = 0, On Error < 0
	 * @return Number bytes read.
	 */
	int readData(char * data, int maxSize, int &errorCode);

	/**
	 * @brief readAllData Read all file data from current postion.
	 * @param blockSize - Size of read blocks.
	 * @param errorCode - Success = 0, Error < 0
	 * @return
	 */
	std::string readAllData(int blockSize, int &errorCode);

	/**
	 * @brief close Closes archive/file.
	 */
	void close();

	/**
	 * @brief reset Closes and reopens file/archive.
	 *
	 * Similar to, but slower than seek(0)
	 */
	void reset();

	/**
	 * @brief isClosed Checks for file/archive not open.
	 * @return true if file closed.
	 */
	bool isClosed();

	/**
	 * @brief exists Checks if the archive/file existant.
	 * @return true if existant (and open).
	 */
	bool exists() const;

	/**
	 * @brief archiveExists Check if is archive, and achives exists
	 * @return true if archive (not file) has been opened.
	 */
	bool archiveExists() const;

	/**
	 * @brief fileName The file name of the last archive entry.
	 * @return entryPath as passed to Ctor(). Zero length if simple file.
	 */
	std::string fileName() const;

	/**
	 * @brief extension The file extension of the last archive entry.
	 * @return Extension suffice of entryPath as passed to Ctor(). Zero length if simple file.
	 */
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
