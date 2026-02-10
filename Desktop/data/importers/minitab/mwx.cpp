#include "mwx.h"
#include "minitabimportcolumn.h"
#include <QFileInfo>
#include "log.h"
#include "columnutils.h"
#include "archivereader.h"

#include <QFileInfo>
#include <QString>
#include <QDebug>

Minitab::Minitab(const std::string &path) : _path(path) {}

void Minitab::parseMwx()
{
	/**
	 * MWX Files Structure
	 * 
	 * / (root)
	 * ├── sheet_metadata*.json    [Which is include metadata of this file]
	 * └── sheets/
	 *     └── 0/
	 *         └── sheet.json         [The data sheet, actually we access the first sheet from Worksheet Uri]
	 */
	
	std::string metadataPath = findMetadataPath();
	
	_metadata = readJsonFromArchive(metadataPath);
	
	if (!_metadata.isMember("Worksheet") || !_metadata["Worksheet"].isMember("Uri"))
		throw std::runtime_error("Invalid MWX File: Missing Worksheet in it, please make sure it is a Minitab worksheet file created by Minitab version 19 or later");
	
	std::string uri = _metadata["Worksheet"]["Uri"].asString();
	
	_sheetRoot = readJsonFromArchive(uri + ".json" );
	
	if (!_sheetRoot.isMember("Data"))
		throw std::runtime_error("MWX missing 'Data' node");
	
	// Seems row count and col count was stored in json already, we just get it.
	if (_sheetRoot.isMember("MaxRows_DEP"))
		_numRows = static_cast<uint32_t>(_sheetRoot["MaxRows_DEP"].asUInt());
	else
		_numRows = 0;
	
	if (_sheetRoot.isMember("MaxColumns_DEP"))
		_numCols = static_cast<uint16_t>(_sheetRoot["MaxColumns_DEP"].asUInt());
	else
		_numCols = 0;

	Log::log() << "MWX Size: rows=" << _numRows << " cols=" << _numCols << std::endl;
}

void Minitab::getColumns(std::vector<MwxImportColumn *> &outColumns, ImportDataSet *dataSet)
{
		const Json::Value &data = _sheetRoot["Data"];
		const Json::Value &jsonCols = data["Columns"];
		
		std::vector<std::string> usedNames;
	
		for (Json::ArrayIndex i = 0; i < jsonCols.size(); ++i)
		{
			const Json::Value &jsonCol = jsonCols[i];

			if (!jsonCol.isMember("WorksheetVarBody"))
					continue;

			const Json::Value &varBody = jsonCol["WorksheetVarBody"];

			std::string name = varBody.get("Name", "V" + std::to_string(i + 1)).asString();
			std::string title = varBody.get("Desc", "").asString();
			
			// distinguish names
			std::string finalName = name;
			int suffix = 1;
			while (std::find(usedNames.begin(), usedNames.end(), finalName) != usedNames.end())
			{
				finalName = name + "_" + std::to_string(suffix++);
			}
			usedNames.push_back(finalName);


			stringvec currentLevels;
			std::map<std::string, std::string> textToIdMap;
			
			if (varBody.isMember("Ordering"))
			{
				const Json::Value &ordering = varBody["Ordering"];
				const Json::Value &textOrder = ordering["TextOrder"]; // Here get levels from json
				
				if (textOrder.isArray() && !textOrder.empty())
				{
					for (const auto &item : textOrder)
					{
						std::string label = item["Key"].asString();
						double val = item["Value"].asDouble();
						currentLevels.push_back(label);
						textToIdMap[label] = std::to_string(static_cast<int>(val));
					}
				}
			}

			const Json::Value &varDataBody = varBody["VarData"]["VarDataBody"];
			columnType colType = columnType::unknown;
			
			if (!currentLevels.empty())
				colType = columnType::nominal;
			
			if (varDataBody.isMember("TextData"))
				colType = columnType::nominal;
			else
				colType = columnType::scale;

			MwxImportColumn *impCol = new MwxImportColumn(dataSet, name, currentLevels, colType);

			if (varDataBody.isMember("TextData"))
			{
				for (const auto &val : varDataBody["TextData"])
				{
					std::string rawStr = val.asString();
					if (textToIdMap.count(rawStr))
							impCol->addValue(textToIdMap[rawStr]);
					else
							impCol->addValue(rawStr);
				}
			}
			else if (varDataBody.isMember("NumericData"))
			{
				for (const auto &val : varDataBody["NumericData"])
				{
					if (val.isNull())
							impCol->addValue("");
					else
					{
							impCol->addValue(ColumnUtils::doubleToStringMaxPrec(val.asDouble(), false));
					}
				}
			}

			impCol->finish(false);
			outColumns.push_back(impCol);
		}
}

Json::Value Minitab::readJsonFromArchive(const std::string &entryPath)
{
		ArchiveReader reader;
		reader.openEntry(_path, entryPath);
	
		if (!reader.exists())
				throw std::runtime_error("Entry not found: " + entryPath);
	
		int errorCode = 0;
		std::string content = reader.readAllData(65536, errorCode);
	
		if (errorCode < 0)
				throw std::runtime_error("Read failed: " + entryPath);
	
		Json::Value root;
		Json::CharReaderBuilder builder;
		std::string errs;
	
		std::unique_ptr<Json::CharReader> jsonReader(builder.newCharReader());
		if (!jsonReader->parse(content.c_str(),
													 content.c_str() + content.size(),
													 &root,
													 &errs))
		{
				throw std::runtime_error("JSON parse error: " + errs);
		}
	
		return root;
}


std::string Minitab::findMetadataPath()
{
		std::vector<std::string> entries = ArchiveReader::getEntryPaths(_path);
	
		for (const std::string& fullPath : entries)
		{
				Log::log() << "MWX file entries:" << fullPath;
				
				if (fullPath.back() == '/')
						continue;
	
				QFileInfo		fi(QString::fromStdString(fullPath));
				QString			baseName = fi.fileName();
				QString			suffix = fi.suffix();
	
				if (!baseName.startsWith("sheet_metadata"))
						continue;
	
				if (suffix != "json")
						continue;
				
				return fullPath;
		}
	
		throw std::runtime_error("Invalid MWX: No sheet_metadata*.json found");
}


