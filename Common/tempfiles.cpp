//
// Copyright (C) 2013-2026 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
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

const long 				outOfDateDelta = 24 * 3600;
long					TempFiles::_sessionId		= 0;
std::string				TempFiles::_sessionDirName	= "";
std::string				TempFiles::_statusFileName	= "";
int						TempFiles::_nextFileId		= 0;
int						TempFiles::_nextTmpFolderId	= 0;

void TempFiles::init(long sessionId)
{
	_sessionId		= sessionId;
	_nextFileId		= 0;
	_sessionDirName	= Dirs::tempDir() + "/" + std::to_string(sessionId);
	_statusFileName	= _sessionDirName +  "/status";
	
	createSessionDir();
}

void TempFiles::createSessionDir()
{
	Log::log() << "DEBUG createSessionDir: Creating session dir at " << _sessionDirName << std::endl;
	
	std::error_code error;

	std::filesystem::path sessionPath = _sessionDirName;
	
	Log::log() << "'" << sessionPath.string() << "' about to be (removed and re)created." << std::endl;

	std::filesystem::remove_all(sessionPath, error);
	std::filesystem::create_directories(sessionPath, error);

	std::fstream f;
	f.open(_statusFileName.c_str(), ios_base::out);
	f.close();

	//std::filesystem::path clipboardPath = clipboard;
	//if ( ! std::filesystem::exists(clipboardPath, error))
	//	std::filesystem::create_directories(clipboardPath, error);
}

void TempFiles::clearSessionDir()
{
	std::filesystem::path sessionPath = _sessionDirName;
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
			if(pathComp.find("tmp") != std::string::npos || pathComp == "status" || pathComp == "internal.sqlite")
				leaveMeBe = true;
		}

		if(!leaveMeBe)
			deleteUs.push_back(it.path());
	}

	for(auto & dir : deleteUs)
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
	std::filesystem::path dir = id >= 0 ? std::filesystem::path(_sessionDirName) / "resources" / std::to_string(id) : std::filesystem::path(_sessionDirName);
	std::filesystem::remove_all(dir, error);
}


void TempFiles::deleteOrphans()
{
	Log::log() << "TempFiles::deleteOrphans started" << std::endl;

	std::error_code error;

	try
	{
		std::filesystem::path tempPath		= Dirs::tempDir();
		std::filesystem::path sessionPath	= _sessionDirName; 
		stringvec aliveIDs;

		std::filesystem::directory_iterator itr(tempPath, error);

		if (error)
		{
			Log::log() << error.message() << std::endl;
			return;
		}

		//find the Dirs that must be deleted and store their PIDs (name)
		for (; itr != std::filesystem::directory_iterator(); itr++)
		{
			std::filesystem::path p = itr->path();

			//Log::log() << "looking at file " << p.string() << std::endl;

			if (p.compare(sessionPath) == 0)
				continue;

			string fileName		= p.filename().generic_string();
			bool is_directory	= std::filesystem::is_directory(p, error);

			if (error)
				continue;

			if (is_directory)
			{
				if (std::atoi(fileName.c_str()) == 0)
					continue;

				std::filesystem::path statusFile = p / "status";

				if (std::filesystem::exists(statusFile, error))
				{
					int64_t modTime	= Utils::getFileModificationTime(statusFile),
							now		= Utils::currentSeconds();

					if (now - modTime > outOfDateDelta)
					{
						Log::log() << "Removing folder because status indicates it is out of date: " << statusFile.string() << std::endl;
						
						std::filesystem::remove_all(p, error);
						if (error)
							Log::log() << "Error when deleting directory: " << error.message() << std::endl;
					}
					else
						aliveIDs.push_back(p.filename().string());
				}
				else // no status file
				{
					std::filesystem::remove_all(p, error);
					if (error)
						Log::log() << "Error when deleting directory, had no status file and " << error.message() << std::endl;
				}
			}
		}

		//Delete files in the root not associated with the IDs that have been active for x time
		deleteStrayRootFiles(aliveIDs, outOfDateDelta);

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

string TempFiles::createSpecific(const string &dir, const string &filename)
{
	std::error_code error;
	string fullPath			= _sessionDirName + "/" + dir;
	std::filesystem::path path	= fullPath;

	if (!std::filesystem::exists(path, error) || error)
		 std::filesystem::create_directories(path, error);

	return fullPath + "/" + filename;
}

void TempFiles::createSpecific(const string &name, int id, string &root, string &relativePath)
{
	root					= _sessionDirName;
	relativePath			= "resources" + (id >= 0 ? "/" + std::to_string(id) : "");
	std::filesystem::path path = std::filesystem::path(root) / relativePath;

	std::error_code error;
	if (!std::filesystem::exists(path, error) || error)
		 std::filesystem::create_directories(path, error);

	relativePath += "/" + name;
}

bool TempFiles::stateFileExists(int id)
{
	std::filesystem::path stateFilePath = (id >= 0) ? (std::filesystem::path(_sessionDirName) / "resources" / std::to_string(id) / "state") : (std::filesystem::path(_sessionDirName) / "resources" / "state");
		
	std::error_code error;
	return std::filesystem::exists(stateFilePath, error) && !error;
}

std::string TempFiles::analysisResourcePath(int id, const std::string &filename)
{
	return id >= 0
		? _sessionDirName + "/resources/" + std::to_string(id) + "/" + filename
		: _sessionDirName + "/resources/" + filename;
}

void TempFiles::create(const string &extension, int id, string &root, string &relativePath)
{
	std::error_code error;

	root					= _sessionDirName;
	string resources		= root +  "/resources" + (id >= 0 ? "/" + std::to_string(id) : "");

	std::filesystem::path path	= resources;

	if (!std::filesystem::exists(resources, error) || error)
		 std::filesystem::create_directories(resources, error);

	string suffix = extension == "" ? "" : "." + extension;

	do
	{
		relativePath	= "resources/" + (id >= 0 ? std::to_string(id) + "/" : "") + "_" + std::to_string(_nextFileId++) + "_t" + std::to_string(Utils::currentMillis()) + suffix;
		path = std::filesystem::path(root) / relativePath;
	}
	while (std::filesystem::exists(path));
}

std::string TempFiles::createTmpFolder()
{
	std::error_code error;

	while(true)
	{
		std::string tmpFolder	= _sessionDirName + "/tmp" + std::to_string(_nextTmpFolderId++) + "/";
		std::filesystem::path path	= tmpFolder;

		if (!std::filesystem::exists(path, error) || error)
		{
			std::filesystem::create_directories(path, error);
			return tmpFolder;
		}
	}
}

vector<string> TempFiles::retrieveList(int id, const std::string &dir)
{
	vector<string> files;

	std::error_code error;

	std::string baseDir = dir.empty() ? _sessionDirName : dir;

	if (id >= 0)
		baseDir += "/resources/" + std::to_string(id);

	std::filesystem::path path = baseDir;

	std::filesystem::directory_iterator itr(path, error);

	if (error)
		return files;

	std::string sessionPath = std::filesystem::path(dir.empty() ? _sessionDirName : dir).generic_string();

	Log::log() << "TempFiles::retrieveList uses sessionpath " << sessionPath << " and finds: ";

	for (; itr != std::filesystem::directory_iterator(); itr++)
		if (std::filesystem::is_regular_file(itr->status()))
		{
			std::filesystem::path pad = itr->path();
			string absPath = pad.generic_string();
			string relPath = absPath.substr(sessionPath.size()+1);

			Log::log(false) << relPath << " from " << absPath << " || ";

			files.push_back(relPath);
		}

	Log::log(false) << std::endl;

	return files;
}

void TempFiles::deleteList(const vector<string> &files)
{
	std::error_code error;

	for(const string &file : files)
	{
		string absPath		= _sessionDirName + "/" + file;
		std::filesystem::path p	= absPath;

		std::filesystem::remove(p, error);
	}
}

void TempFiles::deleteStrayRootFiles(const stringvec& validIDs, long outOfDateDelta)
{
	std::filesystem::path tempPath = Dirs::tempDir();
	std::error_code error;
	std::filesystem::directory_iterator itr(tempPath, error);

	if (error)
	{
		Log::log() << error.message() << std::endl;
		return;
	}

	for (; itr != std::filesystem::directory_iterator(); itr++)
	{
		std::filesystem::path p = itr->path();

		//Log::log() << "looking at file " << p.string() << std::endl;

		string fileName		= p.filename().generic_string();
		bool is_directory	= std::filesystem::is_directory(p, error);

		if (error)
			continue;

		if (!is_directory)
		{					
			int64_t	modTime	= Utils::getFileModificationTime(p),
					now		= Utils::currentSeconds();

			if (now - modTime <= outOfDateDelta || fileName.substr(0, 5).compare("JASP-") != 0)
				continue;

			bool valid = false;
			for (auto& id : validIDs)
			{
				if (fileName.find(id) != std::string::npos)
				{
					valid = true;
					break;
				}
			}
			if (valid)
				continue;

			Log::log() << "Try to delete: " << fileName << std::endl;
			std::filesystem::remove(p, error);

			if (error)
				Log::log() << "Error when deleting file: " << error.message() << std::endl;
		}
	}
}
