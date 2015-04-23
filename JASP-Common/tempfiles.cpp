
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

	filesystem::path sessionPath = Utils::osPath(_tempfiles_sessionDirName);

	filesystem::remove_all(sessionPath, error);
	filesystem::create_directories(sessionPath, error);

	nowide::fstream f;
	f.open(_tempfiles_statusFileName.c_str(), ios_base::out);
	f.close();
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


void tempfiles_deleteAll()
{
	system::error_code error;

	filesystem::path sessionPath = Utils::osPath(_tempfiles_sessionDirName);

	filesystem::remove_all(sessionPath, error);
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

string tempfiles_createSpecific(const string &name, int id)
{
	stringstream ss;
	system::error_code error;

	ss << _tempfiles_sessionDirName << "/resources";

	if (id >= 0)
		ss << "/" << id;

	string dir = ss.str();
	filesystem::path path = Utils::osPath(dir);

	if (filesystem::exists(path, error) == false || error)
		filesystem::create_directories(path, error);

	ss << "/";
	ss << name;

	return ss.str();
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

