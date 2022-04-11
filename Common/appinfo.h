//
// Copyright (C) 2018 University of Amsterdam
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

#ifndef APPINFO_H
#define APPINFO_H

#include "version.h"


///
/// Some useful static information about JASP, when was it build and from what. Etc.
///
class AppInfo
{
public:
	static const Version version;
	static const std::string name;
	static const std::string builddate;
	static const std::string gitBranch;
	static const std::string gitCommit;

	static std::string getShortDesc();
	static std::string getBuildYear();
	static std::string getRVersion();
	static std::string getRDirName();
	static std::string getSigningIdentity();
	static long long   getSimpleCryptKey();
};

#endif // APPINFO_H

