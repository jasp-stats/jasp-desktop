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

#ifndef ARCHIVEREADER_H
#define ARCHIVEREADER_H

#include <string>
#include <vector>

#include <stdlib.h>

#include <archive.h>

/**
 * @brief The ArchiveReader class - Reads archives.
 *
 * Reads and extracts entiries from the archive.
 *
 */
class ArchiveReader
{
public:
	ArchiveReader(const std::string &archivePath, const std::string &entryPath);

	~ArchiveReader();

	/**
	 * @brief size Sizeof archive, or entry.
	 * @return size found.
	 */
	int size() const { return _exists ? _size : 0;
	}

	/**
	 * @brief pos The current position in the file.
	 * @return Bytes from start of file.
	 */
	int pos() const { return _currentRead; }

	/**
	 * @brief bytes Available Bytes in the file or entry.
	 * @return Number bytes still to be read.
	 */
	int bytesAvailable() const { return _exists ? _size - _currentRead : 0;	}


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
	 * @brief isOpen Checks for file/archive being open.
	 * @return true if file opened...
	 */
	bool isOpen() {return _isOpen; }

	/**
	 * @brief exists Checks if the archive/file existant.
	 * @return true if existant (and open).
	 */
	bool exists() const { return _exists; }

	/**
	 * @brief archiveExists Check if is archive, and achives exists
	 * @return true if archive (not file) has been opened.
	 */
	bool archiveExists() const { return _archiveExists; }

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

	struct archive			*	_archive		= nullptr;

	bool						_isOpen			= false,
								_exists			= false,
								_archiveExists	= false;
	int							_size			= 0,
								_currentRead	= 0;
	std::string					_archivePath,
								_entryPath;

	void openEntry(const std::string &archivePath, const std::string &entryPath);
};

#endif // ARCHIVEREADER_H
