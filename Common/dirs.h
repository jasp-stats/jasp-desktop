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

#ifndef DIRS_H
#define DIRS_H

#include <string>
#include <stdexcept>

class Dirs
{
public:

	static std::string appDataDir();
	static std::string tempDir();
	static std::string exeDir();
	static std::string rHomeDir();
	static std::string resourcesDir();


	class Exception : public std::runtime_error
	{
	public:
		Exception(const std::string &message, std::runtime_error &)
			: runtime_error(message.c_str()) { }

		Exception(const std::string &message)
			: runtime_error(message.c_str()) { }
	};

};

#endif // DIRS_H
