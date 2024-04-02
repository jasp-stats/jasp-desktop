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

#include "jaspimporter.h"
#include "columnutils.h"
#include <fstream>

#include <sys/stat.h>

#include <fcntl.h>

//#include "libzip/config.h"
#include <archive.h>
#include <archive_entry.h>
#include <json/json.h>
#include "archivereader.h"
#include "tempfiles.h"
#include "../exporters/jaspexporter.h"

#include "resultstesting/compareresults.h"
#include "log.h"

void JASPImporter::loadDataSet(const std::string &path, std::function<void(int)> progressCallback)
{	
	JASPTIMER_RESUME(JASPImporter::loadDataSet INIT);

	DataSetPackage * packageData = DataSetPackage::pkg();

	packageData->setIsJaspFile(true);

	readManifest(path);

	switch(isCompatible())
	{
	case Compatibility::NotCompatible:
		throw std::runtime_error("The file version is too new.\nPlease update to the latest version of JASP to view this file.");

	case Compatibility::Limited:
			packageData->setWarningMessage("This file was created by a newer version of JASP and may not have complete functionality.");
		break;

	default:
		break;
	}

	JASPTIMER_STOP(JASPImporter::loadDataSet INIT);

	packageData->beginLoadingData();
	loadDataArchive(path, progressCallback);
	loadJASPArchive(path, progressCallback);
	packageData->endLoadingData();
}

JASPImporter::Compatibility JASPImporter::isCompatible(const std::string &path)
{
	try
	{
		readManifest(path);
		return isCompatible();
	}
	catch(...)
	{
		return Compatibility::NotCompatible;
	}
}

void JASPImporter::loadDataArchive(const std::string &path, std::function<void(int)> progressCallback)
{
	JASPTIMER_SCOPE(JASPImporter::loadDataArchive_1_00);

    //Store sqlite into tempfiles:
    ArchiveReader(path, DatabaseInterface::singleton()->dbFile(true)).writeEntryToTempFiles([&](float p){ progressCallback(33.333 * p); });
	
	DataSetPackage::pkg()->loadDataSet([&](float p){ progressCallback(33.333 + 33.333 * p); });

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
}

void JASPImporter::loadJASPArchive(const std::string &path, std::function<void(int)> progressCallback)
{
	JASPTIMER_SCOPE(JASPImporter::loadJASPArchive_1_00 read analyses.json);
	Json::Value analysesData;

	if (parseJsonEntry(analysesData, path, "analyses.json", false))
	{
		stringvec resources = ArchiveReader::getEntryPaths(path, "resources");
	
		double resourceCounter = 0;
		for (const std::string & resource : resources)
		{
			ArchiveReader   resourceEntry = ArchiveReader(path, resource);
			std::string     filename 	  = resourceEntry.fileName(),
							dir			  = resource.substr(0, resource.length() - filename.length() - 1),
							destination   = TempFiles::createSpecific(dir, resourceEntry.fileName());

			resourceEntry.writeEntryToTempFiles(); //this one doesnt really need to give feedback as the files are pretty tiny

            progressCallback( 66.666 + int((33.333 / double(resources.size())) * ++resourceCounter));// "Loading Analyses",
		}
	}

	JASPTIMER_STOP(JASPImporter::loadJASPArchive_1_00 read analyses.json);
	

	JASPTIMER_RESUME(JASPImporter::loadJASPArchive_1_00 packageData->setAnalysesData(analysesData));
	DataSetPackage::pkg()->setAnalysesData(analysesData);
	JASPTIMER_STOP(JASPImporter::loadJASPArchive_1_00 packageData->setAnalysesData(analysesData));
	


	progressCallback(100); //"Initializing Analyses & Results",
}


void JASPImporter::readManifest(const std::string &path)
{
	bool            foundVersion		= false;
	std::string     manifestName		= "manifest.json";
	ArchiveReader	manifestReader;
	manifestReader.openEntry(path, manifestName); //separate from constructor to avoid a failed close (because an exception in constructor messes up destructor)
	int             size				= manifestReader.bytesAvailable(),
					errorCode;

	if (size > 0)
	{
		std::string manifestStr = manifestReader.readAllData(sizeof(char), errorCode);

		if (errorCode != 0)
			throw std::runtime_error("Could not read manifest of JASP archive.");

		Json::Reader    parser;
		Json::Value     manifest;
		parser.parse(manifestStr, manifest);

		std::string jaspArchiveVersionStr	= manifest.get("jaspArchiveVersion", "").asString();
		std::string jaspVersionStr			= manifest.get("jaspVersion",		"").asString();

		foundVersion = ! jaspArchiveVersionStr.empty();

		DataSetPackage::pkg()->setArchiveVersion(	Version(jaspArchiveVersionStr));
		DataSetPackage::pkg()->setJaspVersion(		Version(jaspVersionStr));
	}

	if ( ! foundVersion)
		throw std::runtime_error("Archive missing version information.");
}

bool JASPImporter::parseJsonEntry(Json::Value &root, const std::string &path,  const std::string &entry, bool required)
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

JASPImporter::Compatibility JASPImporter::isCompatible()
{
	if (DataSetPackage::pkg()->archiveVersion().major()		> JASPExporter::jaspArchiveVersion.major() )
		return Compatibility::NotCompatible;

	if (DataSetPackage::pkg()->archiveVersion().minor()		> JASPExporter::jaspArchiveVersion.minor() )
		return Compatibility::Limited;

	return Compatibility::Compatible;
}



