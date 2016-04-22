//
// Copyright (C) 2016 University of Amsterdam
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


const Version AppInfo::version = Version(0, 7, 5, 261);
const std::string AppInfo::name = "JASP";
const std::string AppInfo::builddate = "Thu Apr 21 2016 13:04:43 GMT+0100 (CET)";

std::string AppInfo::getShortDesc()
{
	return AppInfo::name + " " + AppInfo::version.asString();
}
