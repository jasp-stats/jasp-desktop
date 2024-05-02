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

#include "jaspimporterold.h"
#include "data/filtermodel.h"
#include <fstream>

#include <sys/stat.h>

#include <fcntl.h>

//#include "libzip/config.h"
#include <archive.h>
#include <archive_entry.h>
#include <json/json.h>
#include "archivereader.h"
#include "columnutils.h"
#include "tempfiles.h"
#include "resultstesting/compareresults.h"

const Version JASPImporterOld::maxSupportedJaspArchiveVersion = Version("3.1.0");

void JASPImporterOld::loadDataSet(const std::string &path, std::function<void(int)> progressCallback)
{	
	JASPTIMER_RESUME(JASPImporter::loadDataSet INIT);

	DataSetPackage * packageData = DataSetPackage::pkg();

	packageData->setIsJaspFile(true);

	readManifest(path);

	Compatibility compatibility = isCompatible();

	if (compatibility == Compatibility::NotCompatible)	throw std::runtime_error("The file version is too new.\nPlease update to the latest version of JASP to view this file.");
	else if (compatibility == Compatibility::Limited)	packageData->setWarningMessage("This file was created by a newer version of JASP and may not have complete functionality.");

	JASPTIMER_STOP(JASPImporter::loadDataSet INIT);

	packageData->beginLoadingData();
	loadDataArchive(path, progressCallback);
	loadJASPArchive(path, progressCallback);
	packageData->endLoadingData();
}

JASPImporterOld::Compatibility JASPImporterOld::isCompatible(const std::string &path)
{
	try
	{
		readManifest(path);
		return isCompatible();
	}
	catch(const std::runtime_error &)
	{
		return Compatibility::NotCompatible;
	}
}


void JASPImporterOld::loadDataArchive(const std::string &path, std::function<void(int)> progressCallback)
{
	loadDataArchive_1_00(path, progressCallback);
}

void JASPImporterOld::loadDataArchive_1_00(const std::string &path, std::function<void(int)> progressCallback)
{
	JASPTIMER_SCOPE(JASPImporter::loadDataArchive_1_00);

	DataSetPackage * packageData = DataSetPackage::pkg();
	bool success = false;

	Json::Value metaData,
				xData;

	int	columnCount = 0,
		rowCount = 0;

	parseJsonEntry(metaData, path, "metadata.json", true);

	parseJsonEntry(xData, path, "xdata.json", false);

	Json::Value &dataSetDesc			= metaData["dataSet"];

	packageData->createDataSet();

	packageData->setDataFilePath(		metaData.get("dataFilePath",		"")				.asString());
	packageData->setDataFileReadOnly(	metaData.get("dataFileReadOnly",	false)			.asBool());
	packageData->setDataFileTimestamp(	metaData.get("dataFileTimestamp",	0)				.asUInt());
	packageData->setDatabaseJson(		metaData.get("database",			Json::nullValue));

	packageData->dataSet()->setDataFileSynch(true);

	DataSetPackage::filter()->setRFilter(	metaData.get("filterData",			FilterModel::defaultRFilter())	.asString());

	Json::Value jsonFilterConstructor = metaData.get("filterConstructorJSON", DEFAULT_FILTER_JSON);
	DataSetPackage::filter()->setConstructorJson(jsonFilterConstructor.isObject() ? jsonFilterConstructor.toStyledString() : jsonFilterConstructor.asString());

	
	std::map<std::string, intstrmap> columnNameToMissingData;
	
	{
		const Json::Value	& emptyValuesJson	= metaData["emptyValues"],
							& emptyValuesMap	= dataSetDesc["emptyValuesMap"];
		
		stringset emptyValues;
		
		if (emptyValuesJson.isNull())
			// Really old JASP files: the empty values were '.', 'NaN' & 'nan'
			emptyValues = {"NaN", "nan", "."};
		else
			for (const Json::Value & emptyValueJson  : emptyValuesJson)
				emptyValues.insert(emptyValueJson.asString());
	
		
		for(const std::string & columnName : emptyValuesMap.getMemberNames())
			for(const std::string & rowAsString : emptyValuesMap[columnName].getMemberNames())
			{
				int row;
				
				if(ColumnUtils::getIntValue(rowAsString, row))
				{
					const std::string &	missingValue			= emptyValuesMap[columnName][rowAsString].asString();
					columnNameToMissingData[columnName][row]	= missingValue;
					emptyValues									. insert(missingValue);
				}
			}
		packageData->setWorkspaceEmptyValues(emptyValues, false);
	}
		
	columnCount = dataSetDesc["columnCount"].asInt();
	rowCount	= dataSetDesc["rowCount"].asInt();
	if (rowCount < 0 || columnCount < 0)
		throw std::runtime_error("Data size has been corrupted.");


	packageData->setDataSetSize(columnCount, rowCount);

	unsigned long long	progress,
						lastProgress = -1;

	Json::Value &columnsDesc = dataSetDesc["fields"];
	int i = 0;
	std::map<std::string, std::map<int, int> > mapNominalTextValues;

	for (const Json::Value & columnDesc : columnsDesc)
	{
		columnLabelsFromJsonForJASPFile(xData, columnDesc, i);

		progress = (33.0 * i) / columnCount;
		if (progress != lastProgress)
		{
			progressCallback(progress); //fq(tr("Loading Data Set Description")),
			lastProgress = progress;
		}

		i++;
	}

	std::string entryName = "data.bin";
	ArchiveReader dataEntry = ArchiveReader(path, entryName);
	if (!dataEntry.exists())
		throw std::runtime_error("Entry " + entryName + " could not be found.");

	char buff[sizeof(double) > sizeof(int) ? sizeof(double) : sizeof(int)];

	stringvec	values, labels;
	int c=0;
	
	for(Column * column : packageData->dataSet()->columns())
	{
		bool					isScalar	= column->type() == columnType::scale;
		int						typeSize	= isScalar ? sizeof(double) : sizeof(int);
		std::map<int, int>	&	mapValues	= mapNominalTextValues[column->name()];
								values		. clear();
								labels		. clear();
								values		. reserve(rowCount);
								labels		. reserve(rowCount);

		for (size_t r = 0; r < rowCount; r++)
		{
			int errorCode	= 0;
			int size		= dataEntry.readData(buff, typeSize, errorCode);

			if (errorCode != 0 || size != typeSize)
				throw std::runtime_error("Could not read 'data.bin' in JASP archive.");

			if (isScalar)
			{
				values.push_back(ColumnUtils::doubleToString(*reinterpret_cast<double*>(buff)));
				labels.push_back(values.back());
			}
			else
			{
				int value = *reinterpret_cast<int*>(buff);

				Label * label = nullptr;
				
				if (value != EmptyValues::missingValueInteger)
					label = column->labelByIntsId(value-1); //-1 to avoid 1-based nonsense!
				
				else if(columnNameToMissingData.count(column->name()) && columnNameToMissingData[column->name()].count(r))
				{
					const std::string & missingValue = columnNameToMissingData[column->name()][r];
                                        label = column->labelByDisplay(missingValue);
					
					if(!label)
						label = column->labelByIntsId(column->labelsAdd(missingValue));
					
					assert(label);
				}
			
				if(label)
				{
					values.push_back(label->originalValueAsString());
					labels.push_back(label->labelIgnoreEmpty());
				}
				else if (value == EmptyValues::missingValueInteger)
				{
					values.push_back("");
					labels.push_back("");
				}
				else
				{
					values.push_back(std::to_string(value));
					labels.push_back(std::to_string(value));
				}
			}

			progress = 33.0 + ((33.0 * ((c * rowCount) + (r + 1))) / (columnCount * rowCount));
			if (progress != lastProgress)
			{
				progressCallback(progress); // fq(tr("Loading Data Set")),
				lastProgress = progress;
			}
			c++;
		}
		
		column->setValues(values, labels, 0);

	}

	dataEntry.close();

	if(resultXmlCompare::compareResults::theOne()->testMode())
	{
		//Read the results from when the JASP file was saved and store them in compareResults field

		ArchiveReader	resultsEntry	= ArchiveReader(path, "index.html");
		int				errorCode		= 0;
		std::string		html			= resultsEntry.readAllData(sizeof(char), errorCode);

		if (errorCode != 0)
			throw std::runtime_error("Could not read result from 'index.html' in JASP archive.");

		resultXmlCompare::compareResults::theOne()->setOriginalResult(QString::fromStdString(html));
	}

	packageData->dataSet()->loadOldComputedColumnsJson(metaData.get("computedColumns", Json::arrayValue));

	std::vector<bool> filterVector;
	for(const Json::Value & filteredRow : dataSetDesc.get("filterVector", Json::arrayValue))
		filterVector.push_back(filteredRow.asBool());
	packageData->setFilterVectorWithoutModelUpdate(filterVector);

	//Filter should be run if filterVector was not filled and either of the filters was different from default.
	bool filterShouldBeRun =
			filterVector.size() == 0 &&
			(	metaData.get("filterData",				FilterModel::defaultRFilter()).asString()	!= FilterModel::defaultRFilter()
			||	metaData.get("filterConstructorJSON",	DEFAULT_FILTER_JSON).asString()				!= DEFAULT_FILTER_JSON	);

	packageData->setFilterShouldRunInit(filterShouldBeRun);
}

void JASPImporterOld::loadJASPArchive(const std::string &path, std::function<void(int)> progressCallback)
{
	if (DataSetPackage::pkg()->archiveVersion().major() >= 1 && DataSetPackage::pkg()->archiveVersion().major() <= 3) //2.x version have a different analyses.json structure but can be loaded using the 1_00 loader. 3.x adds computed columns
		loadJASPArchive_1_00(path, progressCallback);
	else
		throw std::runtime_error("The file version is not supported.\nPlease update to the latest version of JASP to view this file.");
}

void JASPImporterOld::loadJASPArchive_1_00(const std::string &path, std::function<void(int)> progressCallback)
{
	JASPTIMER_SCOPE(JASPImporter::loadJASPArchive_1_00 read analyses.json);
	Json::Value analysesData;

	progressCallback(66); // "Loading Analyses",

	if (parseJsonEntry(analysesData, path, "analyses.json", false))
	{
		std::vector<std::string> resources = ArchiveReader::getEntryPaths(path, "resources");
	
		double resourceCounter = 0;
		for (std::string resource : resources)
		{
			ArchiveReader resourceEntry = ArchiveReader(path, resource);
			std::string filename	= resourceEntry.fileName();
			std::string dir			= resource.substr(0, resource.length() - filename.length() - 1);

			JASPTIMER_RESUME(JASPImporter::loadJASPArchive_1_00 Create resource files);

			JASPTIMER_RESUME(JASPImporter::loadJASPArchive_1_00 TempFiles::createSpecific);
			std::string destination = TempFiles::createSpecific(dir, resourceEntry.fileName());
			JASPTIMER_STOP(JASPImporter::loadJASPArchive_1_00 TempFiles::createSpecific);
	
			JASPTIMER_RESUME(JASPImporter::loadJASPArchive_1_00 Write file stream);
			std::ofstream file(destination.c_str(),  std::ios::out | std::ios::binary);

			static char streamBuff[8192 * 32];
			file.rdbuf()->pubsetbuf(streamBuff, sizeof(streamBuff)); //Set the buffer manually to make it much faster our issue https://github.com/jasp-stats/INTERNAL-jasp/issues/436 and solution from:  https://stackoverflow.com/a/15177770
	
			static char copyBuff[8192 * 4];
			int			bytes		= 0,
						errorCode	= 0;

			do
			{
				JASPTIMER_RESUME(JASPImporter::loadJASPArchive_1_00 Write file stream - read data);
				bytes = resourceEntry.readData(copyBuff, sizeof(copyBuff), errorCode);
				JASPTIMER_STOP(JASPImporter::loadJASPArchive_1_00 Write file stream - read data);

				if(bytes > 0 && errorCode == 0)
				{
					JASPTIMER_RESUME(JASPImporter::loadJASPArchive_1_00 Write file stream - write to stream);
					file.write(copyBuff, bytes);
					JASPTIMER_STOP(JASPImporter::loadJASPArchive_1_00 Write file stream - write to stream);
				}
				else break;
			}
			while (true);

			file.flush();
			file.close();
			JASPTIMER_STOP(JASPImporter::loadJASPArchive_1_00 Write file stream);
	
			if (errorCode != 0)
				throw std::runtime_error("Could not read resource files.");

			JASPTIMER_STOP(JASPImporter::loadJASPArchive_1_00 Create resource files);

			progressCallback( 67 + int((33.0 / double(resources.size())) * ++resourceCounter));// "Loading Analyses",
		}
	}

	JASPTIMER_STOP(JASPImporter::loadJASPArchive_1_00 read analyses.json);
	
	JASPTIMER_RESUME(JASPImporter::loadJASPArchive_1_00 packageData->setAnalysesData(analysesData));
	DataSetPackage::pkg()->setAnalysesData(analysesData);
	JASPTIMER_STOP(JASPImporter::loadJASPArchive_1_00 packageData->setAnalysesData(analysesData));

	progressCallback(100); //"Initializing Analyses & Results",
}


void JASPImporterOld::readManifest(const std::string &path)
{
	bool            foundVersion		= false;
	std::string     manifestName		= "META-INF/MANIFEST.MF";
	ArchiveReader	manifest;
	manifest.openEntry(path, manifestName);  //separate from constructor to avoid a failed close (because an exception in constructor messes up destructor)
	int             size				= manifest.bytesAvailable();

	if (size > 0)
	{
		char *	data		= new char[size];
		int		startOffset = manifest.pos(),
				errorCode	= 0;

		while (manifest.readData(&data[manifest.pos() - startOffset], 8016, errorCode) > 0 && errorCode == 0) ;

		if (errorCode < 0)
			throw std::runtime_error("Error reading Entry 'manifest.mf' in JASP archive.");

		std::string doc(data, size);
		delete[] data;

		std::stringstream st(doc);
		std::string line;
		while (std::getline(st, line))
		{
			if (line.find("JASP-Archive-Version: ") == 0)
			{
				foundVersion = true;
				DataSetPackage::pkg()->setArchiveVersion(Version(line.substr(22)));
				break;
			}
		}
	}

	if ( ! foundVersion)
		throw std::runtime_error("Archive missing version information.");
}

bool JASPImporterOld::parseJsonEntry(Json::Value &root, const std::string &path,  const std::string &entry, bool required)
{
	//Not particularly happy about the way we need to add a delete at every return here. Would be better to not use `new` and just instantiate a scoped var
	//But that would require removing some `std::runtime_error` from `openEntry` in the `ArchiveReader` constructor. 
	// And this is not the time to rewrite too many things.
	ArchiveReader * dataEntry = NULL;
	try
	{
		dataEntry = new ArchiveReader(path, entry);
	}
	catch(...)
	{
		return false;
	}
	
	if (!dataEntry->archiveExists())
	{
		delete dataEntry;
		throw std::runtime_error("The selected JASP archive '" + path + "' could not be found.");
	}

	if (!dataEntry->exists())
	{
		delete dataEntry;
		
		if (required)
			throw std::runtime_error("Entry '" + entry + "' could not be found in JASP archive.");

		return false;
	}

	int size = dataEntry->bytesAvailable();
	if (size > 0)
	{
		char *data = new char[size];
		int startOffset = dataEntry->pos();
		int errorCode = 0;
		while (dataEntry->readData(&data[dataEntry->pos() - startOffset], 8016, errorCode) > 0 && errorCode == 0) ;

		if (errorCode < 0)
		{
			delete dataEntry;
			throw std::runtime_error("Could not read Entry '" + entry + "' in JASP archive.");
		}

		Json::Reader jsonReader;
		jsonReader.parse(data, (char*)(data + (size * sizeof(char))), root);

		delete[] data;
	}

	dataEntry->close();

	delete dataEntry;
	return true;
}

JASPImporterOld::Compatibility JASPImporterOld::isCompatible()
{
	if (DataSetPackage::pkg()->archiveVersion().major()		> maxSupportedJaspArchiveVersion.major() )
		return Compatibility::NotCompatible;

	if (DataSetPackage::pkg()->archiveVersion().minor()		> maxSupportedJaspArchiveVersion.minor() )
		return Compatibility::Limited;

	return Compatibility::Compatible;
}

columnType JASPImporterOld::parseColumnTypeForJASPFile(const std::string & name)
{
	if (name == "Nominal")				return  columnType::nominal;
	else if (name == "NominalText")		return  columnType::nominalText;
	else if (name == "Ordinal")			return  columnType::ordinal;
	else if (name == "Continuous")		return  columnType::scale;
	else								return  columnType::unknown;
}

void JASPImporterOld::columnLabelsFromJsonForJASPFile(Json::Value xData, Json::Value columnDesc, size_t columnIndex)
{
	assert(!xData.isNull());
	
	std::string     name				= columnDesc["name"].asString();
	Json::Value &   orgStringValuesDesc	= xData[name]["orgStringValues"],
				&   labelsDesc			= xData[name]["labels"];
	Column		*	column				= DataSetPackage::pkg()->dataSet()->column(columnIndex);
	columnType		columnType			= parseColumnTypeForJASPFile(columnDesc["measureType"].asString());

	column->setName(name);
	column->setType(columnType == columnType::nominalText ? columnType::nominal : columnType);
	
	std::map<int, Label*>	labels;

	for (Json::Value & keyValueFilterTrip : labelsDesc)
	{
		int zero			= 0; //MSVC complains on int(0) with: error C2668: 'Json::Value::get': ambiguous call to overloaded function
		int labelValue		= keyValueFilterTrip.get(zero,		Json::nullValue).asInt();		//Is what is in "ints"
		std::string label	= keyValueFilterTrip.get(1,			Json::nullValue).asString();	//Is label, if it is different from the "orgStringValue" then look there for the value
		bool filterAllow	= keyValueFilterTrip.get(2,			true).asBool();
	
		//Do -1 on labelValue to make sure everything is zero-based from here on out
		labels[labelValue] = column->labelByIntsId(column->labelsAdd(labelValue-1, label, filterAllow, "", columnType == columnType::nominalText ? Json::Value(label) : Json::Value(labelValue)));
	}

	for (Json::Value & keyValuePair : orgStringValuesDesc)
	{
		int zero		= 0; //MSVC complains on int(0) with: error C2668: 'Json::Value::get': ambiguous call to overloaded function
		int labelValue	= keyValuePair.get(zero,	Json::nullValue).asInt();
		std::string val = keyValuePair.get(1,		Json::nullValue).asString();
		
		labels[labelValue]->setOriginalValue(val); 
	}
}


