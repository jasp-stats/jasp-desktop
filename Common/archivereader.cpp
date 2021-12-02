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

#include "archivereader.h"

#include <boost/filesystem.hpp>
#include <boost/algorithm/string/predicate.hpp>

#include <sstream>

#include "libzip/archive_entry.h"

using namespace std;


ArchiveReader::ArchiveReader(const string &archivePath, const string &entryPath)
{
	_entryPath		= entryPath;
	_archivePath	= archivePath;

	openEntry(archivePath, entryPath);
}

ArchiveReader::~ArchiveReader()
{
	close();
}

#ifdef _WIN32
#define ARCHIVE_OPEN_FILENAME_FUNC archive_read_open_filename_w
#define PATH_CONVERSION	boost::nowide::widen
#else
#define ARCHIVE_OPEN_FILENAME_FUNC archive_read_open_filename
#define PATH_CONVERSION
#endif

void ArchiveReader::openEntry(const string &archivePath, const string &entryPath)
{
	boost::filesystem::path pathArchive = PATH_CONVERSION(archivePath);
	
	_archiveExists = boost::filesystem::exists(pathArchive);

	if (_archiveExists)
	{
		_archive = archive_read_new();
		archive_read_support_filter_all(_archive);
		archive_read_support_format_all(_archive);

		int r = ARCHIVE_OPEN_FILENAME_FUNC(_archive, pathArchive.native().c_str(), 10240);

		if (r == ARCHIVE_OK)
		{
					_isOpen = true;
			bool	success = false;

			struct archive_entry *entry;
			while (archive_read_next_header(_archive, &entry) == ARCHIVE_OK)
			{
				if (string(archive_entry_pathname(entry)) == entryPath)//pathEntry.native())
				{
					
					_size	= archive_entry_size(entry);
					_exists = true;
					success = true;
					break;
				}
			}
			if (!success)
				throw runtime_error("No entry (" + entryPath + ") found in archive file.");
		}
		else
			throw runtime_error("Archive entry access failed.");
	}

}


string ArchiveReader::fileName() const
{
	size_t last = _entryPath.find_last_of("/");

	if (last == std::string::npos)
		return _entryPath;

	if (last == _entryPath.length() - 1)
		return string();

	return _entryPath.substr(last + 1);
}

string ArchiveReader::extension() const
{
	string filename = fileName();
	size_t last = filename.find_first_of(".");

	if (last == std::string::npos || last == filename.length() - 1)
		return string();

	return filename.substr(last + 1);
}


int ArchiveReader::readData(char *data, int maxSize, int &errorCode)
{
	if (!_exists)
		return 0;

	int bytesAvailable	= _size - _currentRead,
		toRead			= std::min(maxSize, bytesAvailable);

	if (toRead <= 0)
		return 0;

	int count = archive_read_data(_archive, data, toRead);

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

std::string ArchiveReader::readAllData(int blockSize, int &errorCode)
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



void ArchiveReader::close()
{
	if (_isOpen)
	{
		int r = archive_read_free(_archive);
		if (r != ARCHIVE_OK)
			   throw runtime_error("Closing jasp archive failed");
	}

	_isOpen = false;
}

void ArchiveReader::reset()
{
	if (_isOpen)
		close();

	openEntry(_archivePath, _entryPath);
}


vector<string> ArchiveReader::getEntryPaths(const string &archivePath, const string &entryBaseDirectory)
{
	vector<string> files = vector<string>();

	boost::filesystem::path pathArchive = PATH_CONVERSION(archivePath);

	bool archiveExists = boost::filesystem::exists(pathArchive);

	if (archiveExists)
	{
		struct archive *a = archive_read_new();
		archive_read_support_filter_all(a);
		archive_read_support_format_all(a);

		int r = ARCHIVE_OPEN_FILENAME_FUNC(a, pathArchive.native().c_str(), 10240);

		if (r == ARCHIVE_OK)
		{
			struct archive_entry *entry;
			string entryPath;

			while (archive_read_next_header(a, &entry) == ARCHIVE_OK)
			{
				entryPath = string(archive_entry_pathname(entry));
				if (entryBaseDirectory.empty() || boost::starts_with(entryPath, entryBaseDirectory))
					files.push_back(entryPath);
			}
		}

		archive_read_free(a);
	}
	return files;
}
