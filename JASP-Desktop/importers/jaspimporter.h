//
// Copyright (C) 2013-2017 University of Amsterdam
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

#ifndef JASPIMPORTER_H
#define JASPIMPORTER_H


#include "datasetpackage.h"

#include <sysdepfiletype.h>

#include <boost/function.hpp>

#include <string>
#include <vector>

class JASPImporter
{
public:
	enum Compatibility { Compatible, Limited, NotCompatible };

	typedef JaspFiles::Path		FilePath;
	typedef JaspFiles::IFStream	File;

	static void loadDataSet(DataSetPackage *packageData, const FilePath &path, boost::function<void (const std::string &, int)> progressCallback);

private:
	static void loadDataArchive(DataSetPackage *packageData, const FilePath &path, boost::function<void (const std::string &, int)> progressCallback);
	static void loadJASPArchive(DataSetPackage *packageData, const FilePath &path, boost::function<void (const std::string &, int)> progressCallback);
	static void loadDataArchive_1_00(DataSetPackage *packageData, const FilePath &path, boost::function<void (const std::string &, int)> progressCallback);
	static void loadJASPArchive_1_00(DataSetPackage *packageData, const FilePath &path, boost::function<void (const std::string &, int)> progressCallback);

	static Column::ColumnType parseColumnType(std::string name);
	static bool parseJsonEntry(Json::Value &root, const FilePath &path, const std::string &entry, bool required);
	static void readManifest(DataSetPackage *packageData, const FilePath &path);
	static Compatibility isCompatible(DataSetPackage *packageData);
};

#endif // JASPIMPORTER_H
