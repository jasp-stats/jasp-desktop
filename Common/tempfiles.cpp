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
#include <boost/filesystem.hpp>
#include <boost/nowide/convert.hpp>
#include <boost/nowide/fstream.hpp>

#include "columnencoder.h"
#include "utils.h"

#include "dirs.h"
#include "log.h"
using namespace std;
using namespace boost;

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
	
	system::error_code error;

	boost::filesystem::path sessionPath = Utils::osPath(_sessionDirName);
	
	std::cout<< "'" << sessionPath.string() << "' about to be (removed and re)created." << std::endl;

	boost::filesystem::remove_all(sessionPath, error);
	boost::filesystem::create_directories(sessionPath, error);

	nowide::fstream f;
	f.open(_statusFileName.c_str(), ios_base::out);
	f.close();

	//boost::filesystem::path clipboardPath = Utils::osPath(clipboard);
	//if ( ! boost::filesystem::exists(clipboardPath, error))
	//	boost::filesystem::create_directories(clipboardPath, error);
}

void TempFiles::clearSessionDir()
{
	boost::filesystem::path sessionPath = Utils::osPath(_sessionDirName);
	system::error_code error;
	
	if(!boost::filesystem::exists(sessionPath, error) || error)
		return;

	std::vector<boost::filesystem::path> deleteUs;

	for(boost::filesystem::directory_entry & it : boost::filesystem::directory_iterator{sessionPath})
	{
		bool leaveMeBe = false;

		for (const boost::filesystem::path & pp : it.path())
		{
			std::string pathComp = pp.generic_string();
			if(pathComp.find("tmp") != std::string::npos || pathComp == "status")
				leaveMeBe = true;
		}

		if(!leaveMeBe)
			deleteUs.push_back(it.path());
	}

	for(auto dir : deleteUs)
		boost::filesystem::remove_all(dir);
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
	system::error_code error;
	boost::filesystem::path dir = id >= 0 ? _sessionDirName + "/resources/" + std::to_string(id) : Utils::osPath(_sessionDirName);
	boost::filesystem::remove_all(dir, error);
}


void TempFiles::deleteOrphans()
{
	Log::log() << "TempFiles::deleteOrphans started" << std::endl;

	system::error_code error;

	try
	{

		boost::filesystem::path tempPath		= Utils::osPath(Dirs::tempDir());
		boost::filesystem::path sessionPath	= Utils::osPath(_sessionDirName);

		boost::filesystem::directory_iterator itr(tempPath, error);

		if (error)
		{
			Log::log() << error.message() << std::endl;
			return;
		}

		for (; itr != boost::filesystem::directory_iterator(); itr++)
		{
			boost::filesystem::path p = itr->path();

			Log::log() << "looking at file " << p.string() << std::endl;

			if (p.compare(sessionPath) == 0)
				continue;

			string fileName		= Utils::osPath(p.filename());
			bool is_directory	= boost::filesystem::is_directory(p, error);

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
						boost::filesystem::remove(p, error);

						if (error)
							Log::log() << "Error when deleting file: " << error.message() << std::endl;
					}
				}
			}
			else
			{

				if (std::atoi(fileName.c_str()) == 0)
					continue;

				boost::filesystem::path statusFile = Utils::osPath(Utils::osPath(p) + "/status");

				if (boost::filesystem::exists(statusFile, error))
				{
					long modTime	= Utils::getFileModificationTime(Utils::osPath(statusFile));
					long now		= Utils::currentSeconds();

					if (now - modTime > 70)
					{
						boost::filesystem::remove_all(p, error);

						if (error)
							Log::log() << "Error when deleting directory: " << error.message() << std::endl;
					}
				}
				else // no status file
				{
					boost::filesystem::remove_all(p, error);

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
	system::error_code error;
	boost::filesystem::remove_all(Utils::osPath(_clipboard), error);
}

string TempFiles::createSpecific_clipboard(const string &filename)
{
	system::error_code error;

	string fullPath				= _clipboard + "/" + filename;
	boost::filesystem::path	path	= Utils::osPath(fullPath),
						dirPath	= path.parent_path();

	if (!boost::filesystem::exists(dirPath, error) || error)
		 boost::filesystem::create_directories(dirPath, error);

	return fullPath;
}

string TempFiles::createSpecific(const string &dir, const string &filename)
{
	system::error_code error;
	string fullPath			= _sessionDirName + "/" + dir;
	boost::filesystem::path path	= Utils::osPath(fullPath);

	if (!boost::filesystem::exists(path, error) || error)
		 boost::filesystem::create_directories(path, error);

	return fullPath + "/" + filename;
}

void TempFiles::createSpecific(const string &name, int id, string &root, string &relativePath)
{
	root					= _sessionDirName;
	relativePath			= "resources" + (id >= 0 ? "/" + std::to_string(id) : "");
	boost::filesystem::path path	= Utils::osPath(root + "/" + relativePath);

	system::error_code error;
	if (!boost::filesystem::exists(path, error) || error)
		 boost::filesystem::create_directories(path, error);

	relativePath += "/" + name;
}

void TempFiles::create(const string &extension, int id, string &root, string &relativePath)
{
	system::error_code error;

	root					= _sessionDirName;
	string resources		= root +  "/resources" + (id >= 0 ? "/" + std::to_string(id) : "");

	boost::filesystem::path path	= Utils::osPath(resources);

	if (!boost::filesystem::exists(resources, error) || error)
		 boost::filesystem::create_directories(resources, error);

	string suffix = extension == "" ? "" : "." + extension;

	do
	{
		relativePath	= "resources/" + (id >= 0 ? std::to_string(id) + "/" : "") + "_" + std::to_string(_nextFileId++) + "_t" + std::to_string(Utils::currentMillis()) + suffix;
		path			= Utils::osPath(root + "/" + relativePath);
	}
	while (boost::filesystem::exists(path));
}

std::string TempFiles::createTmpFolder()
{
	system::error_code error;

	while(true)
	{
		std::string tmpFolder	= _sessionDirName + "/tmp" + std::to_string(_nextTmpFolderId++) + "/";
		boost::filesystem::path path	= Utils::osPath(tmpFolder);

		if (!boost::filesystem::exists(path, error) || error)
		{
			boost::filesystem::create_directories(path, error);
			return tmpFolder;
		}
	}
}

vector<string> TempFiles::retrieveList(int id)
{
	vector<string> files;

	system::error_code error;

	string dir = _sessionDirName;

	if (id >= 0)
		dir += "/resources/" + std::to_string(id);

	boost::filesystem::path path = Utils::osPath(dir);

	boost::filesystem::directory_iterator itr(path, error);

	if (error)
		return files;

	for (; itr != boost::filesystem::directory_iterator(); itr++)
		if (boost::filesystem::is_regular_file(itr->status()))
		{
			boost::filesystem::path pad = itr->path();
			string absPath = pad.generic_string();
#ifdef _WIN32
			wstring wtest = pad.generic_wstring();
			absPath = nowide::narrow(wtest);
#endif
			string relPath = absPath.substr(_sessionDirName.size()+1);

			files.push_back(relPath);
		}

	return files;
}

void TempFiles::deleteList(const vector<string> &files)
{
	system::error_code error;

	for(const string &file : files)
	{
		string absPath		= _sessionDirName + "/" + file;
		boost::filesystem::path p	= Utils::osPath(absPath);

		boost::filesystem::remove(p, error);
	}
}

