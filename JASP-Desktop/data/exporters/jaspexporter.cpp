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


#include <boost/filesystem.hpp>

#include <sys/stat.h>

#include "dataset.h"

#include "libzip/archive.h"
#include "libzip/archive_entry.h"
#include "jsonredirect.h"
#include "filereader.h"
#include "../JASP-Common/version.h"
#include "tempfiles.h"
#include "appinfo.h"
#include "log.h"
#include "utilenums.h"

const Version JASPExporter::dataArchiveVersion = Version("1.0.2");
const Version JASPExporter::jaspArchiveVersion = Version("3.1.0");


JASPExporter::JASPExporter()
{
	_defaultFileType = Utils::FileType::jasp;
	_allowedFileTypes.push_back(Utils::FileType::jasp);
}

void JASPExporter::saveDataSet(const std::string &path, boost::function<void(int)> progressCallback)
{
	struct archive *a;

	a = archive_write_new();
	archive_write_set_format_zip(a);

#ifdef _WIN32
	int errorCode = archive_write_open_filename_w(a, boost::nowide::widen(path.c_str()).c_str());
#else
	int errorCode = archive_write_open_filename(a, path.c_str());
#endif

	if (errorCode != ARCHIVE_OK)
		throw std::runtime_error("File could not be opened.");

	saveDataArchive(a, progressCallback);
	saveJASPArchive(a, progressCallback);

	errorCode = archive_write_close(a);
	if (errorCode != ARCHIVE_OK)
		throw std::runtime_error("File could not be closed.");

	errorCode = archive_write_free(a);

	progressCallback(100);
}


void JASPExporter::saveDataArchive(archive *a, boost::function<void(int)> progressCallback)
{
	DataSetPackage * package = DataSetPackage::pkg();

	createJARContents(a);

	struct archive_entry *entry;

	int progress,
		lastProgress = -1;

	Json::Value labelsData	= Json::objectValue;
	Json::Value metaData	= Json::objectValue;

	Json::Value &dataSet			= metaData["dataSet"];
	metaData["dataFilePath"]		= Json::Value(package->dataFilePath());
	metaData["dataFileReadOnly"]	= Json::Value(package->dataFileReadOnly());
	metaData["dataFileTimestamp"]	= Json::Value(package->dataFileTimestamp());
	Json::Value emptyValuesJson		= Json::arrayValue;

	const std::vector<std::string>& emptyValuesVector = Utils::getEmptyValues();
	for (auto it : emptyValuesVector)
		emptyValuesJson.append(it);

	metaData["emptyValues"]				= emptyValuesJson;
	metaData["filterData"]				= Json::Value(package->dataFilter());
	metaData["filterConstructorJSON"]	= package->filterConstructorJson();
	metaData["computedColumns"]			= ComputedColumns::singleton()->convertToJson();
	dataSet["rowCount"]					= Json::Value(package->rowCount());
	dataSet["columnCount"]				= Json::Value(package->columnCount());

	dataSet["filterVector"]				= Json::arrayValue;

	for (bool filteredRow : package->filterVector())
		dataSet["filterVector"].append(filteredRow);

	dataSet["emptyValuesMap"]			= Json::objectValue;

	for (auto it : package->emptyValuesMap())
	{
		std::string colName		= it.first;
		auto		map			= it.second;
		Json::Value mapJson		= Json::objectValue;

		for (auto it2 : map)
			mapJson[std::to_string(it2.first)] = it2.second;

		dataSet["emptyValuesMap"][colName] = mapJson;
	}


	Json::Value columnsData = Json::arrayValue;

	//Calculate size of data file that'll be added to the archive
	size_t	dataSize	= 0,
			columnCount	= package->columnCount();

	for (size_t i = 0; i < columnCount; i++)
	{
		columnsData.append(package->columnToJsonForJASPFile(i, labelsData, dataSize));

		progress = 49 * int(i / columnCount);
		if (progress != lastProgress)
		{
			progressCallback(progress);
			lastProgress = progress;
		}
	}
	dataSet["fields"]		= columnsData;

	//Create new entry for archive
	std::string metaDataString	= metaData.toStyledString();
	size_t sizeOfMetaData		= metaDataString.size();
	entry						= archive_entry_new();
	std::string dd2				= std::string("metadata.json");

	archive_entry_set_pathname(entry, dd2.c_str());
	archive_entry_set_size(entry, int(sizeOfMetaData));
	archive_entry_set_filetype(entry, AE_IFREG);
	archive_entry_set_perm(entry, 0644); // Not sure what this does
	archive_write_header(a, entry);

	archive_write_data(a, metaDataString.c_str(), sizeOfMetaData);

	archive_entry_free(entry);


	//Create new entry for archive
	std::string labelDataString = labelsData.toStyledString();
	size_t		sizeOflabelData = labelDataString.size();

	entry = archive_entry_new();
	std::string dd9 = std::string("xdata.json");
	archive_entry_set_pathname(entry, dd9.c_str());
	archive_entry_set_size(entry, int(sizeOflabelData));
	archive_entry_set_filetype(entry, AE_IFREG);
	archive_entry_set_perm(entry, 0644); // Not sure what this does
	archive_write_header(a, entry);

	archive_write_data(a, labelDataString.c_str(), sizeOflabelData);

	archive_entry_free(entry);


	//Create new entry for archive NOTE: must be done before data is added
	entry = archive_entry_new();
	std::string dd = std::string("data.bin");
	archive_entry_set_pathname(entry, dd.c_str());
	archive_entry_set_size(entry, int(dataSize));
	archive_entry_set_filetype(entry, AE_IFREG);
	archive_entry_set_perm(entry, 0644); // Not sure what this does
	archive_write_header(a, entry);

	//Data data to archive

	for (size_t i = 0; i < columnCount; i++)
	{
		if (package->getColumnType(i) != columnType::scale)
			for (const int & value : package->getColumnDataInts(i))
				archive_write_data(a, reinterpret_cast<const char*>(&value), sizeof(int));
		else
			for (const double & value : package->getColumnDataDbls(i))
				archive_write_data(a, reinterpret_cast<const char*>(&value), sizeof(double));

		progress = 49 + 50 * int(i / columnCount);
		if (progress != lastProgress)
		{
			progressCallback(progress);
			lastProgress = progress;
		}
	}

	archive_entry_free(entry);
	
	DataSetPackage::pkg()->waitForExportResultsReady();

	//Create new entry for archive: HTML results
	std::string html = package->analysesHTML();
	size_t htmlSize = html.size();
	entry = archive_entry_new();
	std::string dd3 = std::string("index.html");
	archive_entry_set_pathname(entry, dd3.c_str());
	archive_entry_set_size(entry, int(htmlSize));
	archive_entry_set_filetype(entry, AE_IFREG);
	archive_entry_set_perm(entry, 0644); // Not sure what this does
	archive_write_header(a, entry);

	size_t ws = archive_write_data(a, html.c_str(), htmlSize);
	if (ws != size_t(htmlSize))
		throw std::runtime_error("Can't save jasp archive writing ERROR");

	archive_entry_free(entry);

}

void JASPExporter::saveJASPArchive(archive *a, boost::function<void(int)>)
{
	if (DataSetPackage::pkg()->hasAnalyses())
	{
		struct archive_entry *entry;

		const Json::Value &analysesJson = DataSetPackage::pkg()->analysesData();

		//Create new entry for archive NOTE: must be done before data is added
		std::string analysesString = analysesJson.toStyledString();
		size_t sizeOfAnalysesString = analysesString.size();

		entry = archive_entry_new();
		std::string dd4 = std::string("analyses.json");
		archive_entry_set_pathname(entry, dd4.c_str());
		archive_entry_set_size(entry, int(sizeOfAnalysesString));
		archive_entry_set_filetype(entry, AE_IFREG);
		archive_entry_set_perm(entry, 0644); // Not sure what this does
		archive_write_header(a, entry);

		archive_write_data(a, analysesString.c_str(), sizeOfAnalysesString);

		archive_entry_free(entry);

		char imagebuff[8192];

		Json::Value analysesDataList = analysesJson;
		if (!analysesDataList.isArray())
			analysesDataList = analysesJson["analyses"];

		for (Json::Value::iterator iter = analysesDataList.begin(); iter != analysesDataList.end(); iter++)
		{
			Json::Value &analysisJson = *iter;
			std::vector<std::string> paths = TempFiles::retrieveList(analysisJson["id"].asInt());
			for (size_t j = 0; j < paths.size(); j++)
			{
				FileReader fileInfo = FileReader(TempFiles::sessionDirName() + "/" + paths[j]);
				if (fileInfo.exists())
				{
					int imageSize = fileInfo.size();

					entry = archive_entry_new();
					std::string dd4 = paths[j];
					archive_entry_set_pathname(entry, dd4.c_str());
					archive_entry_set_size(entry, imageSize);
					archive_entry_set_filetype(entry, AE_IFREG);
					archive_entry_set_perm(entry, 0644); // Not sure what this does
					archive_write_header(a, entry);

					int	bytes		= 0,
						errorCode	= 0;

					while ((bytes = fileInfo.readData(imagebuff, sizeof(imagebuff), errorCode)) > 0 && errorCode == 0) {
						archive_write_data(a, imagebuff, size_t(bytes));
					}

					archive_entry_free(entry);

					if (errorCode < 0)
						throw std::runtime_error("Required resource files could not be accessed.");
				}
				else
					Log::log() << "JASP Export: cannot find file " << (TempFiles::sessionDirName() + "/" + paths[j]);
				fileInfo.close();
			}
		}
	}
}

void JASPExporter::createJARContents(archive *a)
{
	struct archive_entry *entry = archive_entry_new();

	std::stringstream manifestStream;
	manifestStream << "Manifest-Version: 1.0" << "\n";
	manifestStream << "Created-By: " << AppInfo::getShortDesc() << "\n";
	manifestStream << "Data-Archive-Version: " << dataArchiveVersion.asString() << "\n";
	manifestStream << "JASP-Archive-Version: " << jaspArchiveVersion.asString() << "\n";

	manifestStream.flush();

	const std::string& tmp	= manifestStream.str();
	size_t manifestSize		= tmp.size();
	const char* manifest	= tmp.c_str();

	std::string f1 = std::string("META-INF/MANIFEST.MF");
	archive_entry_set_pathname(entry, f1.c_str());
	archive_entry_set_size(entry, int(manifestSize));
	archive_entry_set_filetype(entry, AE_IFREG);
	archive_entry_set_perm(entry, 0644); // Not sure what this does
	archive_write_header(a, entry);

	archive_write_data(a, manifest, manifestSize);

	archive_entry_free(entry);
}



