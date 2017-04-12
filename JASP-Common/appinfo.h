//
// Copyright (C) 2017 University of Amsterdam
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

const std::string CURRENT_R_VERSION = "3.3";

class AppInfo
{
public:
	static const Version version;
	static const std::string name;
	static const std::string builddate;

	static std::string getShortDesc();
	static std::string getBuildYear();
};

#endif // APPINFO_H

