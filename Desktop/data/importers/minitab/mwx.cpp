#include "mwx.h"
#include "minitabimportcolumn.h"
#include "log.h"
#include "archivereader.h"
#include <QFileInfo>
#include <QString>

Minitab::Minitab(const std::string &path) : _filePath(path) {}

void Minitab::parseMwx()
{
	std::string metadataPath = findMetadataPath();
	_metadataJson = readJsonFromArchive(metadataPath);

	std::string uri = getSheetUri();

	_sheetJson = readJsonFromArchive(uri + ".json");

	if (!_sheetJson.isMember("Data"))
		throw std::runtime_error("Invalid Minitab file: Missing 'Data' node.");

	_numRows = _sheetJson.get("MaxRows_DEP", 0).asUInt();
	_numCols = _sheetJson.get("MaxColumns_DEP", 0).asUInt();

	Log::log() << "Minitab File Parsed: rows=" << _numRows << " cols=" << _numCols << std::endl;
}

std::string Minitab::getSheetUri() const
{
	QFileInfo fi(QString::fromStdString(_filePath));
	QString ext = fi.suffix().toLower();

	if (ext == "mwx")
		return _metadataJson["Worksheet"]["Uri"].asString();
	else if (ext == "mpx")
		return _metadataJson["Worksheets"]["Items"][0]["Uri"].asString();
	else
		throw std::runtime_error("Unsupported file extension: " + ext.toStdString());
}

void Minitab::getColumns(std::vector<MwxImportColumn *> &columns, ImportDataSet *dataSet)
{
	const Json::Value &jsonCols = _sheetJson["Data"]["Columns"];
	std::vector<std::string> names;

	for (Json::ArrayIndex i = 0; i < jsonCols.size(); ++i)
	{

		const Json::Value &jsonCol = jsonCols[i];
		const Json::Value &varBody = jsonCol["WorksheetVarBody"];

		// according to https://support.minitab.com/en-us/minitab/help-and-how-to/manipulate-data-in-worksheets-columns-and-rows/how-to/using-worksheets/work-with-columns/
		// seems name rules is valid in JASP
		std::string name = varBody.get("Name", "V" + std::to_string(i + 1)).asString();
		names.push_back(name);
		
		std::string colDesc = varBody.get("Desc", name).asString();
		
		stringvec levels;
		std::map<std::string, std::string> textToIdMap;
		parseOrdering(varBody, levels, textToIdMap);

		const Json::Value &varDataBody = varBody["VarData"]["VarDataBody"];

		columnType type = columnType::unknown;
		if (varDataBody.isMember("TextData"))
		{
			if (!levels.empty())
				type = columnType::ordinal;
		}
		else if (varDataBody.isMember("NumericData"))
			type = columnType::scale;

		auto impCol = new MwxImportColumn(dataSet, name, levels, type);

		impCol->setTitle(colDesc);
		
		if (varDataBody.isMember("TextData"))
		{
			for (const auto &val : varDataBody["TextData"])
			{
				std::string rawStr = val.asString();
				impCol->addValue(textToIdMap.count(rawStr) ? textToIdMap[rawStr] : rawStr);
			}
		}
		else if (varDataBody.isMember("NumericData"))
		{
			for (const auto &val : varDataBody["NumericData"])
				impCol->addValue(val.isNull() ? "" : val.asString());
		}

		while (impCol->size() < _numRows)
		{
				impCol->addValue("");  //filling to keep col length consistently
		}

		columns.push_back(impCol);
	}
}

void Minitab::parseOrdering(const Json::Value &varBody, stringvec &levels, std::map<std::string, std::string> &textToIdMap) const
{
	if (varBody.isMember("Ordering"))
	{
		const Json::Value &textOrder = varBody["Ordering"]["TextOrder"];
		if (textOrder.isArray())
		{
			for (const auto &item : textOrder)
			{
				std::string label = item["Key"].asString();
				levels.push_back(label);
				textToIdMap[label] = std::to_string(static_cast<int>(item["Value"].asDouble()));
			}
		}
	}
}

Json::Value Minitab::readJsonFromArchive(const std::string &entryPath)
{
	ArchiveReader reader;
	reader.openEntry(_filePath, entryPath);

	int errorCode = 0;
	std::string content = reader.readAllData(reader.bytesAvailable(), errorCode);

	if (errorCode < 0 || content.empty())
		throw std::runtime_error("Read failed or empty entry: " + entryPath);

	Json::Value root;
	Json::CharReaderBuilder builder;
	std::string errs;
	std::unique_ptr<Json::CharReader> jsonReader(builder.newCharReader());

	if (!jsonReader->parse(content.c_str(), content.c_str() + content.size(), &root, &errs))
		throw std::runtime_error("JSON parse error in " + entryPath + ": " + errs);

	return root;
}

std::string Minitab::findMetadataPath()
{
	/**
	 * MWX Files Structure
	 *
	 * / (root)
	 * ├── sheet_metadata*.json 	[Which is include metadata of this file]
	 * └── sheets/
	 *     └── 0/
	 *         └── sheet.json		[The data sheet, actually we access the first sheet from Worksheet Uri]
	 */

	/**
	 * MPX Files Structure
	 *
	 * / (root)
	 * ├── project_metadata*.json 	[Which is include metadata of this file]
	 * ├── commands/				[Where some figures graphs and gropus things]
	 * └── sheets/
	 *     └── 0/
	 *         └── sheet.json 		[The data sheet, actually we access the first sheet from Worksheet Uri]
	 */

	auto entries = ArchiveReader::getEntryPaths(_filePath);
	for (const std::string &path : entries)
	{
		if (path.find("_metadata") != std::string::npos && path.find(".json") != std::string::npos)
		{
			return path;
		}
	}
	throw std::runtime_error("Invalid Minitab file: No metadata found.");
}
