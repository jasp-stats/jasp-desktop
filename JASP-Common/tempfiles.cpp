
#include "tempfiles.h"

#include <sstream>
#include <fstream>
#include <boost/filesystem.hpp>
#include <boost/foreach.hpp>

#include "base64.h"
#include "dirs.h"
#include "utils.h"

using namespace std;
using namespace boost;

long tempfiles_sessionId;
string tempfiles_sessionDirName;
string tempfiles_statusFileName;
int tempfiles_nextFileId;

void tempfiles_init(long sessionId)
{
	tempfiles_sessionId = sessionId;
	tempfiles_nextFileId = 0;

	stringstream ss;
	ss << Dirs::tempDir();
	ss << "/";
	ss << sessionId;

	tempfiles_sessionDirName = ss.str();

	ss << "/status";

	tempfiles_statusFileName = ss.str();

	if (filesystem::exists(tempfiles_sessionDirName))
		filesystem::remove_all(tempfiles_sessionDirName);

	filesystem::create_directories(tempfiles_sessionDirName);

	fstream f;
	f.open(tempfiles_statusFileName.c_str(), ios_base::out);
	f.close();
}

void tempfiles_attach(long sessionId)
{
	tempfiles_sessionId = sessionId;
	tempfiles_nextFileId = 0;

	stringstream ss;
	ss << Dirs::tempDir();
	ss << "/";
	ss << sessionId;

	tempfiles_sessionDirName = ss.str();

	ss << "/status";

	tempfiles_statusFileName = ss.str();
}


void tempfiles_deleteAll()
{
	filesystem::remove_all(tempfiles_sessionDirName);
}


void tempfiles_deleteOrphans()
{
	system::error_code error;

	filesystem::directory_iterator itr(Dirs::tempDir(), error);

	if (error)
	{
		perror(error.message().c_str());
		return;
	}

	for (; itr != filesystem::directory_iterator(); itr++)
	{
		filesystem::path p = itr->path();

		if (p.compare(tempfiles_sessionDirName) == 0)
			continue;

		if (std::atoi(filesystem::basename(p).c_str()) == 0)
			continue;

		if (filesystem::is_directory(p, error) == false || error)
			continue;

		filesystem::path statusFile(itr->path().string() + "/status");

		if (filesystem::exists(statusFile, error))
		{
			long modTime = Utils::getFileModificationTime(statusFile.generic_string());
			long now = Utils::currentSeconds();

			if (now - modTime > 70)
			{
				filesystem::remove_all(p, error);

				if (error)
					perror(error.message().c_str());
			}
		}
		else if (error)
		{
			continue;
		}
		else // no status file
		{
			filesystem::remove_all(p, error);

			if (error)
				perror(error.message().c_str());
		}
	}
}


void tempfiles_heartbeat()
{
	Utils::touch(tempfiles_statusFileName);
}


string tempfiles_createSpecific(const string &name, int id)
{
	stringstream ss;

	ss << tempfiles_sessionDirName;

	if (id >= 0)
	   ss << "/" << id;

	string dir = ss.str();

	if (filesystem::exists(dir) == false)
		filesystem::create_directories(dir);

	ss << "/";
	ss << name;

	return ss.str();
}

string tempfiles_create(const string &extension, int id)
{
	stringstream ss, ssn;

	ss << tempfiles_sessionDirName;

	if (id >= 0)
	   ss << "/" << id;

	string dir = ss.str();

	if (filesystem::exists(dir) == false)
		filesystem::create_directories(dir);

	string suffix;
	if (extension != "")
		suffix = string(".") + extension;

	do
	{
		ssn.str("");
		ssn << dir;
		ssn << "/";
		ssn << "_";
		ssn << tempfiles_nextFileId++;
		ssn << suffix;
	}
	while (filesystem::exists(ssn.str()));

	tempfiles_retrieveList(id);

	return ssn.str();
}


vector<string> tempfiles_retrieveList(int id)
{
	vector<string> files;

	system::error_code error;

	string dir;
	if (id >= 0)
	{
		stringstream ss;
		ss << tempfiles_sessionDirName;
		ss << "/";
		ss << id;
		dir = ss.str();
	}
	else
	{
		dir = tempfiles_sessionDirName;
	}

	filesystem::directory_iterator itr(dir, error);

	if (error)
		return files;

	for (; itr != filesystem::directory_iterator(); itr++)
	{
		if (filesystem::is_regular_file(itr->status()))
		{
			string filename = itr->path().filename().generic_string();
			if (filename.at(0) != '_')
				continue;

			files.push_back(itr->path().generic_string());
		}
	}

	return files;
}


void tempfiles_deleteList(const vector<string> &files)
{
	system::error_code error;

	BOOST_FOREACH (const string &file, files)
	{
		(void)files;
		filesystem::path p(file);
		filesystem::remove(p, error);
	}
}

