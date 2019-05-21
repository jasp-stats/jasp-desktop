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

#include "filereader.h"

#include <boost/filesystem.hpp>
#include <boost/algorithm/string/predicate.hpp>

#include <sstream>

#include "libzip/archive_entry.h"

using namespace std;


FileReader::FileReader(const string &archivePath, const string &entryPath)
{
	_isArchive = true;
	_entryPath = entryPath;
	_archivePath = archivePath;

	openEntry(archivePath, entryPath);
}

FileReader::FileReader(const string &path)
{
	_isArchive = false;
	_entryPath = path;

	openFile(path);
}

FileReader::~FileReader()
{
	close();
}


void FileReader::openFile(const string &filePath)
{
	#ifdef _WIN32
	boost::filesystem::path path = boost::nowide::widen(filePath);
	#else
	boost::filesystem::path path = filePath;
	#endif

	_exists = boost::filesystem::exists(path);

	if (_exists)
	{
		_size = boost::filesystem::file_size(path);

		_file = new boost::nowide::ifstream(filePath.c_str(), ios::in | ios::binary);
		if (!_file->is_open())
			throw runtime_error("Archive Entry access failed.");
		else
			_isOpen = true;
	}
}

void FileReader::openEntry(const string &archivePath, const string &entryPath)
{
	bool success = false;
	#ifdef _WIN32
	boost::filesystem::path pathArchive = boost::nowide::widen(archivePath);
	#else
	boost::filesystem::path pathArchive = archivePath;
	#endif

	if ((_archiveExists = boost::filesystem::exists(pathArchive)))
	{
		_archive = archive_read_new();
		archive_read_support_filter_all(_archive);
		archive_read_support_format_all(_archive);

		#ifdef _WIN32
		int r = archive_read_open_filename_w(_archive, pathArchive.native().c_str(), 10240);
		#else
		int r = archive_read_open_filename(_archive, pathArchive.native().c_str(), 10240);
		#endif

		_isOpen = true;
		if (r == ARCHIVE_OK)
		{
			struct archive_entry *entry;
			while (archive_read_next_header(_archive, &entry) == ARCHIVE_OK)
			{
				//#ifdef _WIN32
				//if (wstring(archive_entry_pathname_w(entry)) == pathEntry.native())
				//#else
				if (string(archive_entry_pathname(entry)) == entryPath)//pathEntry.native())
				//#endif
				{
					
					_size = archive_entry_size(entry);
					_exists = true;
					success = true;
					break;
				}
			}
			if (!success)
			{
				stringstream str;
				str << "No entry (" << entryPath << ") found in archive file.";
				throw runtime_error(str.str());
			}
		}
		else
			throw runtime_error("Archive entry access failed.");
	}

}


string FileReader::fileName() const
{
	size_t last = _entryPath.find_last_of("/");

	if (last == std::string::npos)
		return _entryPath;

	if (last == _entryPath.length() - 1)
		return string();

	return _entryPath.substr(last + 1);
}

string FileReader::extension() const
{
	string filename = fileName();
	size_t last = filename.find_first_of(".");

	if (last == std::string::npos || last == filename.length() - 1)
		return string();

	return filename.substr(last + 1);
}

bool FileReader::exists() const
{
	return _exists;
}

bool FileReader::archiveExists() const
{
	return _archiveExists;
}

int FileReader::size() const
{
	if (_exists)
		return _size;

	return 0;
}

int FileReader::bytesAvailable() const
{
	if (_exists)
		return _size - _currentRead;

	return 0;
}

int FileReader::pos() const
{
	return _currentRead;
}

bool FileReader::isSequential() const
{
	return true;
}

int FileReader::readData(char *data, int maxSize, int &errorCode)
{
	if (!_exists)
		return 0;

	int bytesAvailable = _size - _currentRead;
	int toRead = maxSize > bytesAvailable ? bytesAvailable : maxSize;

	if (toRead <= 0)
		return 0;

	int count = 0;
	if (_isArchive)
		count = archive_read_data(_archive, data, toRead);
	else
	{
		_file->read(data, toRead);
		count = _file->gcount();
	}

	if (count > 0)
	{
		_currentRead += count;
		errorCode = 0;
	}
	else
		errorCode = count;

	if (_currentRead >= _size)
		close();

	return count;
}

std::string FileReader::readAllData(int blockSize, int &errorCode)
{
	int size = bytesAvailable();
	if (size == 0)
		return NULL;

	size++;

	char *data = new char[size];
	data[size - 1] = '\0';

	int startOffset = _currentRead;

	errorCode = 0;
	while (readData(&data[_currentRead - startOffset], blockSize, errorCode) > 0 && errorCode == 0);

	std::string out(data);
	delete[] data;

	return out;
}



void FileReader::close()
{
	if (_isOpen)
	{
		if (_isArchive)
		{
			int r = archive_read_free(_archive);
			if (r != ARCHIVE_OK)
                throw runtime_error("Closing jasp archive failed");
		}
		else
			_file->close();
	}

	if (_file != NULL)
	{
		delete _file;
		_file = NULL;
	}

	_isOpen = false;
}

bool FileReader::isClosed()
{
	return !_isOpen;
}

void FileReader::reset()
{
	if (_isOpen)
		close();

	if (_isArchive)
		openEntry(_archivePath, _entryPath);
	else
		openFile(_entryPath);
}


vector<string> FileReader::getEntryPaths(const string &archivePath, const string &entryBaseDirectory)
{
	vector<string> files = vector<string>();

	#ifdef _WIN32
	boost::filesystem::path pathArchive = boost::nowide::widen(archivePath);
	#else
	boost::filesystem::path pathArchive = archivePath;
	#endif

	bool archiveExists = boost::filesystem::exists(pathArchive);

	if (archiveExists)
	{
		struct archive *a = archive_read_new();
		archive_read_support_filter_all(a);
		archive_read_support_format_all(a);

		#ifdef _WIN32
		int r = archive_read_open_filename_w(a, pathArchive.native().c_str(), 10240);
		#else
		int r = archive_read_open_filename(a, pathArchive.native().c_str(), 10240);
		#endif

		if (r == ARCHIVE_OK)
		{
			struct archive_entry *entry;
			while (archive_read_next_header(a, &entry) == ARCHIVE_OK)
			{
				string entryPath = string(archive_entry_pathname(entry));
				if (entryBaseDirectory.empty() || boost::starts_with(entryPath, entryBaseDirectory))
					files.push_back(entryPath);
			}
		}

		archive_read_free(a);
	}
	return files;
}
