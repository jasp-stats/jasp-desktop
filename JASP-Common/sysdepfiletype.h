/*
	Copyright (C) Copyright (C) 2013-2017 University of Amsterdam

	This program is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 2 of the License, or
	(at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program.  If not, see <http://www.gnu.org/licenses/>.


	File created by patrick, on 08-08-2017
	Original file name was
*/

#ifndef SYSDEPFILETYPE_H
#define SYSDEPFILETYPE_H 1


#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>

class JaspFileTypes
{
public:
	typedef boost::filesystem::path		FilePath;
	typedef boost::filesystem::ofstream	OFStream;
	typedef boost::filesystem::ifstream	IFStream;
};

#endif // SYSDEPFILETYPE_H
