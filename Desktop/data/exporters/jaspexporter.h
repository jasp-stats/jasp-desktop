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

#ifndef JASPEXPORTER_H
#define JASPEXPORTER_H

#include "exporter.h"
#include "archive.h"

///
/// To export to *.JASP files
/// Those are basically zips with some json files in there btw
class JASPExporter: public Exporter
{
public:
	static const Version jaspArchiveVersion;
	static const Version dataArchiveVersion;

	JASPExporter();
	void saveDataSet(const std::string &path, boost::function<void (int)> progressCallback) override;

private:
	static void saveDataArchive(archive *a, boost::function<void (int)> progressCallback);
	static void saveJASPArchive(archive *a, boost::function<void (int)> progressCallback);

	static void createJARContents(archive *a);
	static std::string getColumnTypeName(columnType columnType);

	JASPTIMER_CLASS(JASPExporter);
};

#endif // JASPEXPORTER_H
