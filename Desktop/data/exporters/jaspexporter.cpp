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

#include "jaspexporter.h"

#include <sys/stat.h>

#include <ios>
#include <archive.h>
#include <archive_entry.h>
#include <json/json.h>
#include <fstream>
#include "version.h"
#include "tempfiles.h"
#include "log.h"
#include "utilenums.h"
#include "utilities/qutils.h"
#include <fstream>
#include "appinfo.h"


const Version JASPExporter::jaspArchiveVersion = Version("5.0.0");
time_t JASPExporter::_now;

JASPExporter::JASPExporter()
{
	_defaultFileType = Utils::FileType::jasp;
	_allowedFileTypes.push_back(Utils::FileType::jasp);
}

void JASPExporter::saveDataSet(const std::string &path, std::function<void(int)> progressCallback)
{
	struct archive *a;

	_now = time(nullptr); //Give all files same timestamp

	a = archive_write_new();
	archive_write_set_format_zip(a);

#ifdef _WIN32
	if (archive_write_open_filename_w(a, tq(path).toStdWString().c_str()) != ARCHIVE_OK)
#else
	if (archive_write_open_filename(a, path.c_str()) != ARCHIVE_OK)
#endif
		throw std::runtime_error(std::string("File could not be opened because of ") + archive_error_string(a));

	saveManifest(a);    progressCallback(10);
	saveAnalyses(a);    progressCallback(30);
	saveResults(a);     progressCallback(70);
	saveDatabase(a);    progressCallback(100);

	if (archive_write_close(a) != ARCHIVE_OK)
		throw std::runtime_error("File could not be closed.");

	archive_write_free(a);

	//Make sure it is now always considered "loading" in DataSetPackage
	DataSetPackage::pkg()->setLoaded(true);
}

void JASPExporter::saveManifest(archive * a)
{
    Json::Value manifest = Json::objectValue;

	manifest["jaspArchiveVersion"]	= jaspArchiveVersion.asString();
	manifest["jaspVersion"]			= AppInfo::version.asString();

    makeEntry(a, "manifest.json", manifest.toStyledString());
}

void JASPExporter::saveResults(archive * a)
{
	DataSetPackage::pkg()->waitForExportResultsReady();

	makeEntry(a, "index.html", fq(DataSetPackage::pkg()->analysesHTML()));
}

void JASPExporter::saveTempFile(archive *a, const std::string & filePath)
{
	// std::ios::ate seeks to the end of stream immediately after open
	std::ifstream   readTempFile(TempFiles::sessionDirName() + "/" + filePath, std::ios::ate | std::ios::binary);
	char            fileBuff[8192];

	if (readTempFile.is_open())
	{
		archive_entry * entry       = archive_entry_new();

		archive_entry_set_pathname( entry,  filePath.c_str());
		archive_entry_set_size(		entry,	readTempFile.tellg()); // get size from curpos after ios::ate seek
		archive_entry_set_filetype(	entry,	AE_IFREG);
		archive_entry_set_birthtime(entry,  _now, 0);
		archive_entry_set_ctime(    entry,  _now, 0);
		archive_entry_set_mtime(    entry,  _now, 0);
		archive_entry_set_atime(    entry,  _now, 0);
		archive_entry_set_perm(		entry,	0644); //set some read write permissions

		archive_write_header(		a,		entry);

		readTempFile.seekg(0, std::ios::beg);	// move back to begin
		while (!readTempFile.eof())
		{
			readTempFile.read(fileBuff, sizeof(fileBuff));
			archive_write_data(a, fileBuff, readTempFile.gcount());
		}

		archive_entry_free(entry);
	}
	else
		Log::log() << "JASP Export: cannot find/open file " << filePath << std::endl;;

	readTempFile.close();
}

void JASPExporter::makeEntry(archive * a, const std::string & filename, const std::string & data)
{
	archive_entry *entry = archive_entry_new();

	archive_entry_set_pathname(	entry,	filename.c_str());
	archive_entry_set_size(		entry,	int(data.size()));
	archive_entry_set_birthtime(entry,  _now, 0);
	archive_entry_set_ctime(    entry,  _now, 0);
	archive_entry_set_mtime(    entry,  _now, 0);
	archive_entry_set_atime(    entry,  _now, 0);
	archive_entry_set_filetype(	entry,	AE_IFREG);
	archive_entry_set_perm(		entry,	0644);

	archive_write_header(               a,	entry);
	size_t written = archive_write_data(a,  data.c_str(), data.size());

	if(written != data.size())
		throw std::runtime_error("Saving file " + filename + " to jaspFile did not write properly, only " +
			std::to_string(written) + " bytes written while " + std::to_string(data.size()) + " were expected...");

	archive_entry_free(entry);
}

void JASPExporter::saveAnalyses(archive *a)
{
	const Json::Value & analysesJson = DataSetPackage::pkg()->analysesData();

	makeEntry(a, "analyses.json", analysesJson.toStyledString());

	const Json::Value & analysesDataList = analysesJson.isArray() ? analysesJson : analysesJson["analyses"];

	for (const Json::Value & analysisJson : analysesDataList)
		for (const std::string & path : TempFiles::retrieveList(analysisJson["id"].asInt()))
			saveTempFile(a, path);
}

void JASPExporter::saveDatabase(archive * a)
{
	saveTempFile(a, DatabaseInterface::singleton()->dbFile(true));
}
