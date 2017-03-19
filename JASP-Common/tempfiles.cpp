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

#include <sstream>
#include <boost/filesystem.hpp>
#include <boost/foreach.hpp>
#include <boost/nowide/fstream.hpp>

#include "base64.h"
#include "dirs.h"
#include "utils.h"

using namespace std;
using namespace boost;

long _tempfiles_sessionId;
string _tempfiles_sessionDirName;
string _tempfiles_statusFileName;
string _tempfiles_clipboard;
int _tempfiles_nextFileId;

void tempfiles_init(long sessionId)
{
	system::error_code error;

	_tempfiles_sessionId = sessionId;
	_tempfiles_nextFileId = 0;

	stringstream ss;
	ss << Dirs::tempDir();
	ss << "/";
	ss << sessionId;

	_tempfiles_sessionDirName = ss.str();

	ss << "/status";

	_tempfiles_statusFileName = ss.str();

	stringstream ss2;
	ss2 << Dirs::tempDir();
	ss2 << "/";
	ss2 << "clipboard";

	_tempfiles_clipboard = ss2.str();


	filesystem::path sessionPath = Utils::osPath(_tempfiles_sessionDirName);

	filesystem::remove_all(sessionPath, error);
	filesystem::create_directories(sessionPath, error);

	nowide::fstream f;
	f.open(_tempfiles_statusFileName.c_str(), ios_base::out);
	f.close();

	filesystem::path clipboardPath = Utils::osPath(_tempfiles_clipboard);

	//if ( ! filesystem::exists(clipboardPath, error))
	//	filesystem::create_directories(clipboardPath, error);
}

void tempfiles_attach(long sessionId)
{
	_tempfiles_sessionId = sessionId;
	_tempfiles_nextFileId = 0;

	stringstream ss;
	ss << Dirs::tempDir();
	ss << "/";
	ss << sessionId;

	_tempfiles_sessionDirName = ss.str();

	ss << "/status";

	_tempfiles_statusFileName = ss.str();
}


void tempfiles_deleteAll(int id)
{

	system::error_code error;
	filesystem::path dir;

	if (id >= 0)
	{
		stringstream ss;
		ss << _tempfiles_sessionDirName;
		ss << "/";
		ss << "resources/";
		ss << id;
		dir = ss.str();
	}
	else
	{
		dir = Utils::osPath(_tempfiles_sessionDirName);
	}

	filesystem::remove_all(dir, error);
}


void tempfiles_deleteOrphans()
{
	system::error_code error;

	try {

		filesystem::path tempPath = Utils::osPath(Dirs::tempDir());
		filesystem::path sessionPath = Utils::osPath(_tempfiles_sessionDirName);

		filesystem::directory_iterator itr(tempPath, error);

		if (error)
		{
			perror(error.message().c_str());
			return;
		}

		for (; itr != filesystem::directory_iterator(); itr++)
		{
			filesystem::path p = itr->path();

			if (p.compare(sessionPath) == 0)
				continue;

			string name = Utils::osPath(p.filename());

			if (std::atoi(name.c_str()) == 0)
				continue;

			if (filesystem::is_directory(p, error) == false || error)
				continue;

			filesystem::path statusFile = Utils::osPath(Utils::osPath(p) + "/status");

			if (filesystem::exists(statusFile, error))
			{
				long modTime = Utils::getFileModificationTime(Utils::osPath(statusFile));
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
}

void tempfiles_purgeClipboard()
{
	system::error_code error;

	filesystem::path clipboardPath = Utils::osPath(_tempfiles_clipboard);

	filesystem::remove_all(clipboardPath, error);
}

string tempfiles_createSpecific_clipboard(const string &filename)
{
	stringstream ss;
	system::error_code error;

	ss << _tempfiles_clipboard << "/" << filename;

	string fullPath = ss.str();
	filesystem::path path = Utils::osPath(fullPath);

	filesystem::path dirPath = path.parent_path();

	if (filesystem::exists(dirPath, error) == false || error)
		filesystem::create_directories(dirPath, error);

	return fullPath;
}

string tempfiles_createSpecific(const string &dir, const string &filename)
{
	stringstream ss;
	system::error_code error;

	ss << _tempfiles_sessionDirName << "/" << dir;

	string fullPath = ss.str();
	filesystem::path path = Utils::osPath(fullPath);

	if (filesystem::exists(path, error) == false || error)
		filesystem::create_directories(path, error);

	ss << "/";
	ss << filename;

	return ss.str();
}

void tempfiles_createSpecific(const string &name, int id, string &root, string &relativePath)
{
	stringstream ssAbsolute, ssRelative;
	system::error_code error;

	root = _tempfiles_sessionDirName;

	ssAbsolute << root << "/";

	ssAbsolute << "resources";
	ssRelative << "resources";

	if (id >= 0)
	{
		ssRelative << "/" << id;
		ssAbsolute << "/" << id;
	}

	string dir = ssAbsolute.str();
	filesystem::path path = Utils::osPath(dir);

	if (filesystem::exists(path, error) == false || error)
		filesystem::create_directories(path, error);

	ssAbsolute << "/" << name;
	ssRelative << "/" << name;

	relativePath = ssRelative.str();
}

void tempfiles_create(const string &extension, int id, string &root, string &relativePath)
{
	stringstream ssRoot, ssRelative;
	system::error_code error;

	ssRoot << _tempfiles_sessionDirName;

	root = ssRoot.str();

	ssRoot << "/resources";

	if (id >= 0)
		ssRoot << "/" << id;

	string resources = ssRoot.str();

	filesystem::path path = Utils::osPath(resources);

	if (filesystem::exists(resources, error) == false)
		filesystem::create_directories(resources, error);

	string suffix;
	if (extension != "")
		suffix = string(".") + extension;

	do
	{
		ssRelative.str("");
		ssRelative << "resources/";

		if (id >= 0)
			ssRelative << id << "/";

		ssRelative << "_";
		ssRelative << _tempfiles_nextFileId++;
		ssRelative << suffix;

		relativePath = ssRelative.str();
		string fullPath = root + "/" + relativePath;

		path = Utils::osPath(fullPath);
	}
	while (filesystem::exists(path));


}


vector<string> tempfiles_retrieveList(int id)
{
	vector<string> files;

	system::error_code error;

	string dir;
	if (id >= 0)
	{
		stringstream ss;
		ss << _tempfiles_sessionDirName;
		ss << "/";
		ss << "resources/";
		ss << id;
		dir = ss.str();
	}
	else
	{
		dir = _tempfiles_sessionDirName;
	}

	filesystem::path path = Utils::osPath(dir);

	filesystem::directory_iterator itr(path, error);

	if (error)
		return files;

	for (; itr != filesystem::directory_iterator(); itr++)
	{
		if (filesystem::is_regular_file(itr->status()))
		{
			string absPath = itr->path().generic_string();
			string relPath = absPath.substr(_tempfiles_sessionDirName.size()+1);

			files.push_back(relPath);
		}
	}

	return files;
}

string tempfiles_sessionDirName()
{
	return _tempfiles_sessionDirName;
}

void tempfiles_deleteList(const vector<string> &files)
{
	system::error_code error;

	BOOST_FOREACH (const string &file, files)
	{
		(void)files;
		string absPath = _tempfiles_sessionDirName + "/" + file;
		filesystem::path p = Utils::osPath(absPath);
		filesystem::remove(p, error);
	}
}

