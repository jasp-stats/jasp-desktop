//
// Copyright (C) 2013-2017 University of Amsterdam
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

#include "tempfiles.h"
#include "sysdepfiletype.h"

#include <sstream>
#include <boost/foreach.hpp>
#include <iostream>

#include "base64.h"
#include "dirs.h"
#include "utils.h"

using namespace std;
using namespace boost;

namespace bf = boost::filesystem;
long _tempfiles_sessionId;
JaspFileTypes::FilePath _tempfiles_sessionDirName;
JaspFileTypes::FilePath _tempfiles_statusFileName;
JaspFileTypes::FilePath _tempfiles_clipboard;
int _tempfiles_nextFileId;
vector<JaspFileTypes::FilePath> tempfiles_shmemNames;

// Names of dirs under TEMP.
static const string status("status");
static const string resources("resources");
static const string clipboard("clipboard");

void tempfiles_init(long sessionId)
{
	system::error_code error;

	_tempfiles_sessionId = sessionId;
	_tempfiles_nextFileId = 0;

	stringstream ss;
	ss << sessionId;

	_tempfiles_sessionDirName = Dirs::tempDir();
	_tempfiles_sessionDirName  /= ss.str();

    cout << "__tempfiles_sessionDirName, native " << _tempfiles_sessionDirName.string()
		 << ", generic " << _tempfiles_sessionDirName.generic_string() << endl;

	_tempfiles_statusFileName = _tempfiles_sessionDirName.generic_string();
	_tempfiles_statusFileName /= status;

    cout << "_tempfiles_statusFileName, native " << _tempfiles_statusFileName.string()
		 << ", generic " << _tempfiles_statusFileName.generic_string() << endl;

	_tempfiles_clipboard = Dirs::tempDir();
	_tempfiles_clipboard /= clipboard;

    cout << "_tempfiles_clipboard, native " << _tempfiles_clipboard.string()
		 << ", generic " << _tempfiles_clipboard.generic_string() << endl;

	filesystem::remove_all(_tempfiles_sessionDirName, error);
	filesystem::create_directories(_tempfiles_sessionDirName, error);

	JaspFileTypes::OFStream f(_tempfiles_statusFileName, ios::out);
	f.close();

	//filesystem::path clipboardPath = Utils::osPath(_tempfiles_clipboard);

	//if ( ! filesystem::exists(clipboardPath, error))
	//	filesystem::create_directories(clipboardPath, error);
}

void tempfiles_attach(long sessionId)
{
	_tempfiles_sessionId = sessionId;
	_tempfiles_nextFileId = 0;

	_tempfiles_sessionDirName = Dirs::tempDir();
	stringstream ss;
	ss << sessionId;

	_tempfiles_sessionDirName /= ss.str();
	_tempfiles_sessionDirName /= status;
}

void tempfiles_deleteAll(int id)
{

	system::error_code error;
	filesystem::path dir(_tempfiles_sessionDirName);

	if (id >= 0)
	{
		dir.append(resources);
		stringstream ss;
		ss << id;
		dir.append(ss.str());
	}

	filesystem::remove_all(dir, error);
}


void tempfiles_deleteOrphans()
{
	system::error_code error;

	try {

		//filesystem::path tempPath = Utils::osPath(Dirs::tempDir());
		//filesystem::path sessionPath = Utils::osPath(_tempfiles_sessionDirName);

		filesystem::directory_iterator itr(Dirs::tempDir(), error);

		if (error)
		{
			perror(error.message().c_str());
			return;
		}

		for (; itr != filesystem::directory_iterator(); itr++)
		{
			filesystem::path p = itr->path();

			// Dont touch the session dir root.
			if (p.compare(_tempfiles_sessionDirName) == 0)
				continue;

			// Dont touch the shared memeory.
			for (vector<filesystem::path>::const_iterator it = tempfiles_shmemNames.begin(); it != tempfiles_shmemNames.end(); ++it)
			{
				if (p.compare(*it) == 0)
					continue;
			}

			bool is_directory = filesystem::is_directory(p, error);
			if (error)
				continue;

			if (!is_directory)
			{
				if (p.filename().string().substr(0, 5).compare("JASP-") == 0)
				{
					long modTime = Utils::getFileModificationTime(p);
					long now = Utils::currentSeconds();

					std::cout.flush();
					if (now - modTime > 70)
					{
						std::cout << "Try to delete: " << p.filename() << std::endl;
						std::cout.flush();

						filesystem::remove(p, error);

						if (error)
						{
							std::cout << "Error when deleting File: " << error.message() << std::endl;
							std::cout.flush();

							perror(error.message().c_str());
						}
					}
				}
			}
			else
			{

                if (std::atoi(p.filename().string().c_str()) == 0)
					continue;

				filesystem::path statusFile = p;
				statusFile.append(status);

				if (filesystem::exists(statusFile, error))
				{
					long modTime = Utils::getFileModificationTime(statusFile);
					long now = Utils::currentSeconds();

					if (now - modTime > 70)
					{
						filesystem::remove_all(p, error);

						if (error)
							perror(error.message().c_str());
					}
				}
				else // no status file
				{
					filesystem::remove_all(p, error);

					if (error)
						perror(error.message().c_str());
				}
			}
		}

	}
	catch (runtime_error e)
	{
		perror("Could not delete orphans");
		perror(e.what());
		return;
	}
}

void tempfiles_heartbeat()

{
	Utils::touch(_tempfiles_statusFileName);
	for (vector<JaspFileTypes::FilePath>::const_iterator it = tempfiles_shmemNames.begin(); it != tempfiles_shmemNames.end(); ++it)
	{
		Utils::touch(*it);
	}
}


void tempfiles_purgeClipboard()
{
	system::error_code error;

	filesystem::remove_all(_tempfiles_clipboard, error);
}

JaspFileTypes::FilePath tempfiles_createSpecific_clipboard(const JaspFileTypes::FilePath &filename)
{
	system::error_code error;

	filesystem::path fullPath = _tempfiles_clipboard;
	fullPath.append(filename.filename().generic_string());
	filesystem::create_directories(fullPath, error);
	return fullPath;
}


JaspFileTypes::FilePath tempfiles_createSpecificFp(const JaspFileTypes::FilePath &dir, const JaspFileTypes::FilePath &filename)
{
	system::error_code error;

	filesystem::path pa;
	if (dir.is_relative())
	{
		pa = _tempfiles_sessionDirName;
		pa /= dir;
	}
	else
		pa = dir;

	if (filesystem::exists(pa, error) == false || error)
		filesystem::create_directories(pa, error);

	pa /= filename.filename();

	return pa;
}


/**
 * @brief tempfiles_createSpecific Creates specific directory in session directory.
 * @param name The file name to append.
 * @param id The ID to use (-1 if none.)
 * @param root OUT The root directory
 * @param relativePath OUT The relative path.
 *
 * Does the following:
 *  Returns the root dir of the files in "root".
 *  Creates a sub folder "./resources[/<id>].
 *  Creates the sub folder.
 *  Returns the sub-folder in "relativePath"
 */

static void _createDir(int id, JaspFileTypes::FilePath &root, JaspFileTypes::FilePath &relativePath)
{
	system::error_code error;

	root = _tempfiles_sessionDirName;

	JaspFileTypes::FilePath absPath(_tempfiles_sessionDirName);
	relativePath.clear();
	absPath /= resources;
	relativePath /= resources;

	if (id >= 0)
	{
		stringstream ss;
		ss << id;
		absPath /= ss.str();
		relativePath /= ss.str();
	}

	if (filesystem::exists(absPath, error) == false || error)
		filesystem::create_directories(absPath, error);
}

/**
 * @brief tempfiles_createSpecific Creates specific directory in session directory.
 * @param name The file name to append.
 * @param id The ID to use (-1 if none.)
 * @param root OUT The root directory
 * @param relativePath OUT The relative path.
 *
 * Does the following:
 *  Returns the root dir of the files in "root".
 *  Creates a sub folder "./resources[/<id>].
 *  Creates the sub folder.
 *  Appends the name to the sub folder to create relative path.
 *  Returns the sub-folder in "relativePath"
 */
void tempfiles_createSpecific(const JaspFileTypes::FilePath &name, int id, JaspFileTypes::FilePath &root, JaspFileTypes::FilePath &relativePath)
{
	_createDir(id, root, relativePath);
	relativePath /= name.filename();
}

/**
 * @brief tempfiles_create Creates a temporary file in <root>/resources[/<id>] named _NNN where NNN is next available number.
 * @param extension File extention to use.
 * @param id ID for directory (-1 if none).
 * @param root OUT root directory used.
 * @param relativePath OUT relative path of file.
 */
void tempfiles_create(const JaspFileTypes::FilePath &extension, int id, JaspFileTypes::FilePath &root, JaspFileTypes::FilePath &relativePath)
{

	static int _nextFileId = 1;

	_createDir(id, root, relativePath);

	string ext;
	if (extension.extension() != JaspFileTypes::FilePath())
		ext = extension.extension().generic_string();

	JaspFileTypes::FilePath trialPath;
	do
	{
		trialPath = root;
		trialPath /= relativePath;

		stringstream filename;
		filename << "_" << _nextFileId++ << ext;

		trialPath /= filename.str();
	}
	while (filesystem::exists(trialPath));

}

vector<JaspFileTypes::FilePath> tempfiles_retrieveListFullPaths(int id)
{
	vector<JaspFileTypes::FilePath> files;

	system::error_code error;

	JaspFileTypes::FilePath dir(_tempfiles_sessionDirName);

	if (id >= 0)
	{
		stringstream ss;
		ss << id;
		dir.append("resources");
		dir.append(ss.str());
	}

	filesystem::directory_iterator itr(dir, error);
	if (error)
		return files;

	for (; itr != filesystem::directory_iterator(); itr++)
	{
		if (filesystem::is_regular_file(itr->status()))
			files.push_back(itr->path());
	}

	return files;
}


JaspFileTypes::FilePath tempfiles_sessionDirName()
{
	return _tempfiles_sessionDirName;
}
/*
 Moved to Utils::deleteList(...);
void tempfiles_deleteList(const vector<SystemDepFileTypes::FilePath> &files)
{
	system::error_code error;

	BOOST_FOREACH (const SystemDepFileTypes::FilePath &file, files)
	{
//		(void)files;
		string absPath = _tempfiles_sessionDirName + "/" + file;
		filesystem::path p = Utils::osPath(absPath);
		filesystem::remove(p, error);
	}
}

*/
void tempFiles_addShmemFileName(const string &name)
{
	bf::path p(Dirs::tempDir().generic_string());
	p /= name;
	tempfiles_shmemNames.push_back(p);
}

