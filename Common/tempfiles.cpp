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

#include "tempfiles.h"

#include <iostream>
#include <sstream>
#include <fstream>

#include "columnencoder.h"
#include "utils.h"

#include "dirs.h"
#include "log.h"
using namespace std;

long					TempFiles::_sessionId		= 0;
std::string				TempFiles::_sessionDirName	= "";
std::string				TempFiles::_statusFileName	= "";
std::string				TempFiles::_clipboard		= "";
int						TempFiles::_nextFileId		= 0;
int						TempFiles::_nextTmpFolderId	= 0;

void TempFiles::init(long sessionId)
{
	_sessionId		= sessionId;
	_nextFileId		= 0;
	_sessionDirName	= Dirs::tempDir() + "/" + std::to_string(sessionId);
	_statusFileName	= _sessionDirName +  "/status";
	_clipboard		= Dirs::tempDir() + "/clipboard";
	
	createSessionDir();
}

void TempFiles::createSessionDir()
{
	std::cout << "TempFiles::createSessionDir(): ";
	
	std::error_code error;

	std::filesystem::path sessionPath = Utils::osPath(_sessionDirName);
	
	std::cout<< "'" << sessionPath.string() << "' about to be (removed and re)created." << std::endl;

	std::filesystem::remove_all(sessionPath, error);
	std::filesystem::create_directories(sessionPath, error);

	std::fstream f;
	f.open(_statusFileName.c_str(), ios_base::out);
	f.close();

	//std::filesystem::path clipboardPath = Utils::osPath(clipboard);
	//if ( ! std::filesystem::exists(clipboardPath, error))
	//	std::filesystem::create_directories(clipboardPath, error);
}

void TempFiles::clearSessionDir()
{
	std::filesystem::path sessionPath = Utils::osPath(_sessionDirName);
	std::error_code error;
	
	if(!std::filesystem::exists(sessionPath, error) || error)
		return;

	std::vector<std::filesystem::path> deleteUs;

	for(const std::filesystem::directory_entry & it : std::filesystem::directory_iterator{sessionPath})
	{
		bool leaveMeBe = false;

		for (const std::filesystem::path & pp : it.path())
		{
			std::string pathComp = pp.generic_string();
			if(pathComp.find("tmp") != std::string::npos || pathComp == "status")
				leaveMeBe = true;
		}

		if(!leaveMeBe)
			deleteUs.push_back(it.path());
	}

	for(auto dir : deleteUs)
		std::filesystem::remove_all(dir);
}

void TempFiles::attach(long sessionId)
{
	_sessionId		= sessionId;
	_nextFileId		= 0;
	_sessionDirName	= Dirs::tempDir() + "/" + std::to_string(sessionId);
	_statusFileName	= _sessionDirName + "/status";
}


void TempFiles::deleteAll(int id)
{
	std::error_code error;
	std::filesystem::path dir = id >= 0 ? std::filesystem::path(_sessionDirName + "/resources/" + std::to_string(id)) : Utils::osPath(_sessionDirName);
	std::filesystem::remove_all(dir, error);
}


void TempFiles::deleteOrphans()
{
	Log::log() << "TempFiles::deleteOrphans started" << std::endl;

	std::error_code error;

	try
	{

		std::filesystem::path tempPath		= Utils::osPath(Dirs::tempDir());
		std::filesystem::path sessionPath	= Utils::osPath(_sessionDirName);

		std::filesystem::directory_iterator itr(tempPath, error);

		if (error)
		{
			Log::log() << error.message() << std::endl;
			return;
		}

		for (; itr != std::filesystem::directory_iterator(); itr++)
		{
			std::filesystem::path p = itr->path();

			Log::log() << "looking at file " << p.string() << std::endl;

			if (p.compare(sessionPath) == 0)
				continue;

			string fileName		= Utils::osPath(p.filename());
			bool is_directory	= std::filesystem::is_directory(p, error);

			if (error)
				continue;

			if (!is_directory)
			{
				if (fileName.substr(0, 5).compare("JASP-") == 0)
				{
					long modTime	= Utils::getFileModificationTime(Utils::osPath(p));
					long now		= Utils::currentSeconds();

					if (now - modTime > 70)
					{
						Log::log() << "Try to delete: " << fileName << std::endl;
						std::filesystem::remove(p, error);

						if (error)
							Log::log() << "Error when deleting file: " << error.message() << std::endl;
					}
				}
			}
			else
			{

				if (std::atoi(fileName.c_str()) == 0)
					continue;

				std::filesystem::path statusFile = Utils::osPath(Utils::osPath(p) + "/status");

				if (std::filesystem::exists(statusFile, error))
				{
					long modTime	= Utils::getFileModificationTime(Utils::osPath(statusFile));
					long now		= Utils::currentSeconds();

					if (now - modTime > 70)
					{
						std::filesystem::remove_all(p, error);

						if (error)
							Log::log() << "Error when deleting directory: " << error.message() << std::endl;
					}
				}
				else // no status file
				{
					std::filesystem::remove_all(p, error);

					if (error)
						Log::log() << "Error when deleting directory, had no status file and " << error.message() << std::endl;
				}
			}
		}

	}
	catch (runtime_error e)
	{
		Log::log() << "Could not delete orphans, error: " << e.what() << std::endl;
		return;
	}
}


void TempFiles::heartbeat()
{
	Utils::touch(_statusFileName);
}

void TempFiles::purgeClipboard()
{
	std::error_code error;
	std::filesystem::remove_all(Utils::osPath(_clipboard), error);
}

string TempFiles::createSpecific_clipboard(const string &filename)
{
	std::error_code error;

	string fullPath				= _clipboard + "/" + filename;
	std::filesystem::path	path	= Utils::osPath(fullPath),
						dirPath	= path.parent_path();

	if (!std::filesystem::exists(dirPath, error) || error)
		 std::filesystem::create_directories(dirPath, error);

	return fullPath;
}

string TempFiles::createSpecific(const string &dir, const string &filename)
{
	std::error_code error;
	string fullPath			= _sessionDirName + "/" + dir;
	std::filesystem::path path	= Utils::osPath(fullPath);

	if (!std::filesystem::exists(path, error) || error)
		 std::filesystem::create_directories(path, error);

	return fullPath + "/" + filename;
}

void TempFiles::createSpecific(const string &name, int id, string &root, string &relativePath)
{
	root					= _sessionDirName;
	relativePath			= "resources" + (id >= 0 ? "/" + std::to_string(id) : "");
	std::filesystem::path path	= Utils::osPath(root + "/" + relativePath);

	std::error_code error;
	if (!std::filesystem::exists(path, error) || error)
		 std::filesystem::create_directories(path, error);

	relativePath += "/" + name;
}

void TempFiles::create(const string &extension, int id, string &root, string &relativePath)
{
	std::error_code error;

	root					= _sessionDirName;
	string resources		= root +  "/resources" + (id >= 0 ? "/" + std::to_string(id) : "");

	std::filesystem::path path	= Utils::osPath(resources);

	if (!std::filesystem::exists(resources, error) || error)
		 std::filesystem::create_directories(resources, error);

	string suffix = extension == "" ? "" : "." + extension;

	do
	{
		relativePath	= "resources/" + (id >= 0 ? std::to_string(id) + "/" : "") + "_" + std::to_string(_nextFileId++) + "_t" + std::to_string(Utils::currentMillis()) + suffix;
		path			= Utils::osPath(root + "/" + relativePath);
	}
	while (std::filesystem::exists(path));
}

std::string TempFiles::createTmpFolder()
{
	std::error_code error;

	while(true)
	{
		std::string tmpFolder	= _sessionDirName + "/tmp" + std::to_string(_nextTmpFolderId++) + "/";
		std::filesystem::path path	= Utils::osPath(tmpFolder);

		if (!std::filesystem::exists(path, error) || error)
		{
			std::filesystem::create_directories(path, error);
			return tmpFolder;
		}
	}
}

vector<string> TempFiles::retrieveList(int id)
{
	vector<string> files;

	std::error_code error;

	string dir = _sessionDirName;

	if (id >= 0)
		dir += "/resources/" + std::to_string(id);

	std::filesystem::path path = Utils::osPath(dir);

	std::filesystem::directory_iterator itr(path, error);

	if (error)
		return files;

	for (; itr != std::filesystem::directory_iterator(); itr++)
		if (std::filesystem::is_regular_file(itr->status()))
		{
			std::filesystem::path pad = itr->path();
			string absPath = pad.generic_string();
			string relPath = absPath.substr(_sessionDirName.size()+1);

			files.push_back(relPath);
		}

	return files;
}

void TempFiles::deleteList(const vector<string> &files)
{
	std::error_code error;

	for(const string &file : files)
	{
		string absPath		= _sessionDirName + "/" + file;
		std::filesystem::path p	= Utils::osPath(absPath);

		std::filesystem::remove(p, error);
	}
}

