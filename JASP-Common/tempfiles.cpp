
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

long tempfiles_sessionId;
string tempfiles_sessionDirName;
string tempfiles_statusFileName;
int tempfiles_nextFileId;

void tempfiles_init(long sessionId)
{
	system::error_code error;

	tempfiles_sessionId = sessionId;
	tempfiles_nextFileId = 0;

	stringstream ss;
	ss << Dirs::tempDir();
	ss << "/";
	ss << sessionId;

	tempfiles_sessionDirName = ss.str();

	ss << "/status";

	tempfiles_statusFileName = ss.str();

	filesystem::path sessionPath = Utils::osPath(tempfiles_sessionDirName);

	filesystem::remove_all(sessionPath, error);
	filesystem::create_directories(sessionPath, error);

	nowide::fstream f;
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
	system::error_code error;

	filesystem::path sessionPath = Utils::osPath(tempfiles_sessionDirName);

	filesystem::remove_all(sessionPath, error);
}


void tempfiles_deleteOrphans()
{
	system::error_code error;

	try {

		filesystem::path tempPath = Utils::osPath(Dirs::tempDir());
		filesystem::path sessionPath = Utils::osPath(tempfiles_sessionDirName);

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
	Utils::touch(tempfiles_statusFileName);
}


string tempfiles_createSpecific(const string &name, int id)
{
	stringstream ss;
	system::error_code error;

	ss << tempfiles_sessionDirName;

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

string tempfiles_create(const string &extension, int id)
{
	stringstream ss, ssn;
	system::error_code error;

	ss << tempfiles_sessionDirName;

	if (id >= 0)
		ss << "/" << id;

	string dir = ss.str();
	filesystem::path path = Utils::osPath(dir);

	if (filesystem::exists(path, error) == false)
		filesystem::create_directories(path, error);

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

		path = Utils::osPath(ssn.str());
	}
	while (filesystem::exists(path));

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

	filesystem::path path = Utils::osPath(dir);

	filesystem::directory_iterator itr(path, error);

	if (error)
		return files;

	for (; itr != filesystem::directory_iterator(); itr++)
	{
		if (filesystem::is_regular_file(itr->status()))
		{
			string filename = Utils::osPath(itr->path().filename());
			if (filename.at(0) != '_')
				continue;

			files.push_back(Utils::osPath(itr->path()));
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
		filesystem::path p = Utils::osPath(file);
		filesystem::remove(p, error);
	}
}

