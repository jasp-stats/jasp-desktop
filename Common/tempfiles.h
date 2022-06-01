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

#ifndef TEMPFILES_H
#define TEMPFILES_H

#include <string>
#include <vector>

/// Each running UI process creates a series of temporary directories
/// 
/// Within these temporary directories, subdirectories exist, numbered with the analysisId, for each analysis.
/// 
/// In these the state file exists, as well as all the plots. 
/// 
/// In the event that the UI process crashes, and fails to clean up after itself, these 'orphans' are deleted next time the program starts up. 
/// This is performed by the deleteOrphans() function below.
/// During normal operation, the UI process 'touch'es a status file in the temp directory, every 30 seconds or so this is handled by the heartbeat() function below.
/// 
/// deleteOrphans() checks the modification time of this status file and if it is more than a minute old, the temp directory is considered orphaned and is deleted.
/// 
class TempFiles
{
	typedef std::vector<std::string> stringvec;

public:
	static void			init(long _sessionId);
	static void			attach(long _sessionId);
	static void			heartbeat();

	static void			createSessionDir();
	static void			clearSessionDir();

	static std::string	createSpecific_clipboard(const std::string &filename);
	static void			purgeClipboard();

	static void			create(const std::string &extension, int id, std::string &root, std::string &relativePath);
	static void			createSpecific(const std::string &name, int id, std::string &root, std::string &relativePath);
	static std::string	createSpecific(const std::string &dir, const std::string &filename);
	static std::string	createTmpFolder();

	static std::string	sessionDirName() { return _sessionDirName; }
	static stringvec	retrieveList(int id = -1);

	static void			deleteList(const stringvec &files);
	static void			deleteAll(int id = -1);
	static void			deleteOrphans();

private:
						TempFiles() {}
	static long			_sessionId;
	static std::string	_sessionDirName,
						_statusFileName,
						_clipboard;
	static int			_nextFileId,
						_nextTmpFolderId;
};


#endif // TEMPFILES_H
