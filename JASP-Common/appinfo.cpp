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

#include "appinfo.h"
#include <sstream> 

//Put separately for use in JASP/Tools/make-debian-package.sh.
const int VersionMajor(0), VersionMinor(8), VersionRevision(7), VersionBuildNumber(255);

const Version AppInfo::version = Version(VersionMajor, VersionMinor, VersionRevision, VersionBuildNumber);
const std::string AppInfo::name = "JASP";
const std::string AppInfo::builddate = __DATE__ " " __TIME__ " (Netherlands)" ;

std::string AppInfo::getShortDesc()
{
	return AppInfo::name + " " + AppInfo::version.asString();
}

std::string AppInfo::getBuildYear()
{
	std::string datum = __DATE__;
	return datum.substr(datum.length() - 4);
}

std::string AppInfo::getRVersion()
{	
	std::stringstream ss;
	ss << CURRENT_R_VERSION;
	std::string str;
	ss >> str;
	return str;
}

