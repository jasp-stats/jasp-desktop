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

#include "jaspexporter.h"

#include <boost/foreach.hpp>
#include <boost/filesystem.hpp>

#include <sys/stat.h>

#include "dataset.h"

#include "libzip/archive.h"
#include "libzip/archive_entry.h"
#include "lib_json/json.h"
#include "filereader.h"
#include "version.h"
#include "tempfiles.h"
#include "appinfo.h"
#include <QDebug>

using namespace std;

const Version JASPExporter::dataArchiveVersion = Version("1.0.2");
const Version JASPExporter::jaspArchiveVersion = Version("2.0.0");


JASPExporter::JASPExporter() {
	_defaultFileType = Utils::jasp;
    _allowedFileTypes.push_back(Utils::jasp);
}

void JASPExporter::saveDataSet(const std::string &path, DataSetPackage* package, boost::function<void (const std::string &, int)> progressCallback)
{
	struct archive *a;

	a = archive_write_new();
	archive_write_set_format_zip(a);

#ifdef __WIN32__
	int errorCode = archive_write_open_filename_w(a, boost::nowide::widen(path.c_str()).c_str());
#else
	int errorCode = archive_write_open_filename(a, path.c_str());
#endif

	if (errorCode != ARCHIVE_OK)
		throw runtime_error("File could not be opened.");

	saveDataArchive(a, package, progressCallback);
	saveJASPArchive(a, package, progressCallback);

	errorCode = archive_write_close(a);
	if (errorCode != ARCHIVE_OK)
		throw runtime_error("File could not be closed.");

	errorCode = archive_write_free(a);

	progressCallback("Saving Data Set", 100);
}

void JASPExporter::saveDataArchive(archive *a, DataSetPackage *package, boost::function<void (const std::string &, int)> progressCallback)
{
	createJARContents(a);

	struct archive_entry *entry;

	DataSet *dataset = package->dataSet;

	unsigned long long progress;
	unsigned long long lastProgress = -1;

	Json::Value labelsData = Json::objectValue;
	Json::Value metaData = Json::objectValue;

	Json::Value &dataSet = metaData["dataSet"];
	metaData["dataFilePath"] = Json::Value(package->dataFilePath);
	metaData["dataFileReadOnly"] = Json::Value(package->dataFileReadOnly);
	metaData["dataFileTimestamp"] = Json::Value(package->dataFileTimestamp);
	dataSet["rowCount"] = Json::Value(dataset ? dataset->rowCount() : 0);
	dataSet["columnCount"] = Json::Value(dataset ? dataset->columnCount(): 0);
	Json::Value columnsData = Json::arrayValue;

	//Calculate size of data file that'll be added to the archive////////
	int dataSize = 0;
	int columnCount = dataset ? dataset->columnCount() : 0;
	for (int i = 0; i < columnCount; i++)
	{
		Column &column = dataset->column(i);
		string name = column.name();
		Json::Value columnMetaData = Json::Value(Json::objectValue);	
		columnMetaData["name"] = Json::Value(name);
		columnMetaData["measureType"] = Json::Value(getColumnTypeName(column.columnType()));
		if (column.columnType() != Column::ColumnTypeScale)
		{
			columnMetaData["type"] = Json::Value("integer");
			dataSize += sizeof(int) * dataset->rowCount();
		}
		else
		{
			columnMetaData["type"] = Json::Value("number");
			dataSize += sizeof(double) * dataset->rowCount();
		}


		if (column.columnType() != Column::ColumnTypeScale)
		{
			Labels &labels = column.labels();
			if (labels.size() > 0)
			{
				Json::Value &columnLabelData = labelsData[name];
				Json::Value &labelsMetaData = columnLabelData["labels"];
				int labelIndex = 0;

				for (Labels::const_iterator iter = labels.begin(); iter != labels.end(); iter++)
				{
					const Label &label = *iter;
					Json::Value keyValuePair = Json::arrayValue;
					keyValuePair.append(label.value());
					keyValuePair.append(label.text());
					labelsMetaData.append(keyValuePair);
					labelIndex += 1;
				}

				Json::Value &orgStringValuesMetaData = columnLabelData["orgStringValues"];
				std::map<int, std::string> &orgLabels = labels.getOrgStringValues();
				for (std::map<int, std::string>::const_iterator iter = orgLabels.begin(); iter != orgLabels.end(); ++iter)
				{
					const std::pair<int, std::string> &pair = *iter;
					Json::Value keyValuePair = Json::arrayValue;
					keyValuePair.append(pair.first);
					keyValuePair.append(pair.second);
					orgStringValuesMetaData.append(keyValuePair);
				}
			}
		}

		columnsData.append(columnMetaData);

		progress = 49 * (i / columnCount);
		if (progress != lastProgress)
		{
			progressCallback("Saving Meta Data", progress);
			lastProgress = progress;
		}
	}
	dataSet["fields"] = columnsData;


	//Create new entry for archive
	string metaDataString = metaData.toStyledString();
	int sizeOfMetaData = metaDataString.size();

	entry = archive_entry_new();
	string dd2 = string("metadata.json");
	archive_entry_set_pathname(entry, dd2.c_str());
	archive_entry_set_size(entry, sizeOfMetaData);
	archive_entry_set_filetype(entry, AE_IFREG);
	archive_entry_set_perm(entry, 0644); // Not sure what this does
	archive_write_header(a, entry);

	archive_write_data(a, metaDataString.c_str(), sizeOfMetaData);

	archive_entry_free(entry);


	//Create new entry for archive
	string labelDataString = labelsData.toStyledString();
	int sizeOflabelData = labelDataString.size();

	entry = archive_entry_new();
	string dd9 = string("xdata.json");
	archive_entry_set_pathname(entry, dd9.c_str());
	archive_entry_set_size(entry, sizeOflabelData);
	archive_entry_set_filetype(entry, AE_IFREG);
	archive_entry_set_perm(entry, 0644); // Not sure what this does
	archive_write_header(a, entry);

	archive_write_data(a, labelDataString.c_str(), sizeOflabelData);

	archive_entry_free(entry);


	//Create new entry for archive NOTE: must be done before data is added
	entry = archive_entry_new();
	string dd = string("data.bin");
	archive_entry_set_pathname(entry, dd.c_str());
	archive_entry_set_size(entry, dataSize);
	archive_entry_set_filetype(entry, AE_IFREG);
	archive_entry_set_perm(entry, 0644); // Not sure what this does
	archive_write_header(a, entry);

	//Data data to archive

	for (int i = 0; i < columnCount; i++)
	{
		Column &column = dataset->column(i);

		if (column.columnType() != Column::ColumnTypeScale)
		{
			for (Column::Ints::iterator iter = column.AsInts.begin(); iter != column.AsInts.end(); iter++)
			{
				int value = *iter;
				archive_write_data(a, (const char*)(&value), sizeof(int));
			}
		}
		else
		{
			for (Column::Doubles::iterator iter = column.AsDoubles.begin(); iter != column.AsDoubles.end(); iter++)
			{
				double value = *iter;
				archive_write_data(a, (const char*)(&value), sizeof(double));
			}
		}

		progress = 49 + 50 * (i / columnCount);
		if (progress != lastProgress)
		{
			progressCallback("Saving Data Set", progress);
			lastProgress = progress;
		}
	}

	archive_entry_free(entry);

	//Create new entry for archive: HTML results
	string html = package->analysesHTML;
	int htmlSize = html.size();
	entry = archive_entry_new();
	string dd3 = string("index.html");
	archive_entry_set_pathname(entry, dd3.c_str());
	archive_entry_set_size(entry, htmlSize);
	archive_entry_set_filetype(entry, AE_IFREG);
	archive_entry_set_perm(entry, 0644); // Not sure what this does
	archive_write_header(a, entry);

	int ws = archive_write_data(a, html.c_str(), htmlSize);
	if (ws != htmlSize)
		throw runtime_error("Can't save jasp archive writing ERROR");

	archive_entry_free(entry);

}

void JASPExporter::saveJASPArchive(archive *a, DataSetPackage *package, boost::function<void (const std::string &, int)> progressCallback)
{
	if (package->hasAnalyses)
	{
		struct archive_entry *entry;

		Json::Value &analysesJson = package->analysesData;

		//Create new entry for archive NOTE: must be done before data is added
		string analysesString = analysesJson.toStyledString();
		int sizeOfAnalysesString = analysesString.size();

		entry = archive_entry_new();
		string dd4 = string("analyses.json");
		archive_entry_set_pathname(entry, dd4.c_str());
		archive_entry_set_size(entry, sizeOfAnalysesString);
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
			vector<string> paths = tempfiles_retrieveList(analysisJson["id"].asInt());
			for (size_t j = 0; j < paths.size(); j++)
			{
				FileReader fileInfo = FileReader(tempfiles_sessionDirName() + "/" + paths[j]);
				if (fileInfo.exists())
				{
					int imageSize = fileInfo.size();

					entry = archive_entry_new();
					string dd4 = paths[j];
					archive_entry_set_pathname(entry, dd4.c_str());
					archive_entry_set_size(entry, imageSize);
					archive_entry_set_filetype(entry, AE_IFREG);
					archive_entry_set_perm(entry, 0644); // Not sure what this does
					archive_write_header(a, entry);

					int bytes = 0;
					int errorCode = 0;
					while ((bytes = fileInfo.readData(imagebuff, sizeof(imagebuff), errorCode)) > 0 && errorCode == 0) {
						archive_write_data(a, imagebuff, bytes);
					}

					archive_entry_free(entry);

					if (errorCode < 0)
						throw runtime_error("Required resource files could not be accessed.");
				}
				fileInfo.close();
			}
		}
	}
}

void JASPExporter::createJARContents(archive *a)
{
	struct archive_entry *entry = archive_entry_new();

	stringstream manifestStream;
	manifestStream << "Manifest-Version: 1.0" << "\n";
	manifestStream << "Created-By: " << AppInfo::getShortDesc() << "\n";
	manifestStream << "Data-Archive-Version: " << dataArchiveVersion.asString() << "\n";
	manifestStream << "JASP-Archive-Version: " << jaspArchiveVersion.asString() << "\n";

	manifestStream.flush();

	const std::string& tmp = manifestStream.str();
	int manifestSize = tmp.size();
	const char* manifest = tmp.c_str();

	string f1 = string("META-INF/MANIFEST.MF");
	archive_entry_set_pathname(entry, f1.c_str());
	archive_entry_set_size(entry, manifestSize);
	archive_entry_set_filetype(entry, AE_IFREG);
	archive_entry_set_perm(entry, 0644); // Not sure what this does
	archive_write_header(a, entry);

	archive_write_data(a, manifest, manifestSize);

	archive_entry_free(entry);
}


string JASPExporter::getColumnTypeName(Column::ColumnType columnType)
{
	switch(columnType)
	{
	case Column::ColumnTypeNominal:
		return "Nominal";
	case Column::ColumnTypeNominalText:
		return "NominalText";
	case Column::ColumnTypeOrdinal:
		return "Ordinal";
	case Column::ColumnTypeScale:
		return "Continuous";
	default:
		return "Unknown";
	}

}
