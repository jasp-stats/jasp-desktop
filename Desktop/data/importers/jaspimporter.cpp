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
#include "log.h"
#include <fcntl.h>
#include "qutils.h"
#include <archive.h>
#include <sys/stat.h>
#include <archive_entry.h>
#include <json/json.h>
#include "archivereader.h"
#include "tempfiles.h"
#include "../exporters/jaspexporter.h"
#include "resultstesting/compareresults.h"
#include "data/jaspencryptiondata.h"
#include "data/jaspencrypt.h"
#include "utilities/desktopcommunicator.h"
#include "data/asyncloader.h"

const Version JASPImporter::minJaspVersion = Version("0.18.0");

void JASPImporter::loadDataSet(const std::string &path, std::function<void(int)> progressCallback)
{	
	JASPTIMER_RESUME(JASPImporter::loadDataSet INIT);

	DataSetPackage * packageData = DataSetPackage::pkg();

	packageData->setIsJaspFile(true);

	//do some decrypting if necessary
	bool					encrypted	= JASPEncrypt::detectEncryptedJASPFile(path);
	std::filesystem::path	tmpPath		= path;
	
	if(encrypted) 
	{
		try 
		{
			JaspEncryptionData::getInstance()->setEncryptionActive(true);
						
			if (!DesktopCommunicator::singleton()->queryEncryptionSettings(true))
				throw LoaderException("Query Encryption Settings cancelled", true);
			
						tmpPath					= std::filesystem::temp_directory_path() / ("_tmp_unlock_" + std::filesystem::path(path).filename().generic_string());
			Json::Value root;
            std::string	privKey					= JaspEncryptionData::getInstance()->getPrivatekey(),
						responsePublicKey		= "",
						responsePasswordSalt	= "";
			bool		usePrivKey				= privKey.length();
			
			//check if user want to use privkey or password to decrypt
			
            JASPEncrypt::decrypt(tmpPath, path, usePrivKey ? privKey : JaspEncryptionData::getInstance()->getPassword(), root, responsePublicKey, responsePasswordSalt, usePrivKey);
            
            JaspEncryptionData::getInstance()->setPublicKeyResponse(responsePublicKey);
            JaspEncryptionData::getInstance()->setPasswordSaltResponse(responsePasswordSalt);
			
		} catch (LoaderException& e) {
			throw e;
		} catch (std::exception& e) {
			Log::log() << "Decrypt failed: " << e.what() << std::endl;
			throw LoaderException("Decryption failed. Please confirm the password was right. \n\n" + std::string(" Technical Reason: ") + std::string(e.what()));
		}
	}

	switch(isCompatible(tmpPath.generic_string()))
	{
	case Compatibility::NotCompatible:
	{
		if (DataSetPackage::pkg()->jaspVersion() < JASPImporter::minJaspVersion)
			throw LoaderException(
				fq(tr("The JASP file is too old (%1) and is not supported anymore.\n"
					  "Load and save it first in an intermediate JASP version (between %2 and 0.96.1) to upgrade your JASP file to a compatible version")
					   .arg(DataSetPackage::pkg()->jaspVersion().isEmpty() ? "older than " + (JASPImporter::minJaspVersion.asString()) : DataSetPackage::pkg()->jaspVersion().asString())
					   .arg(JASPImporter::minJaspVersion.asString())));
		else
			throw LoaderException("The file version is too new.\nPlease update to the latest version of JASP to view this file.");
	}

	case Compatibility::Limited:
			packageData->setWarningMessage("This file was created by a newer version of JASP and may not have complete functionality.");
		break;

	default:
		break;
	}

	JASPTIMER_STOP(JASPImporter::loadDataSet INIT);

	loadDataArchive(tmpPath.generic_string(), progressCallback);
	loadJASPArchive(tmpPath.generic_string(), progressCallback);

	if(encrypted) //delete the decrypted tmp file we made
		std::filesystem::remove(tmpPath);
}

JASPImporter::Compatibility JASPImporter::isCompatible(const std::string &path)
{
	try
	{
		ManifestInfo info = ArchiveReader::readManifest(path);
		DataSetPackage::pkg()->setArchiveVersion(	Version(info.jaspArchiveVersion));
		DataSetPackage::pkg()->setJaspVersion(		Version(info.jaspVersion));

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
	//ArchiveReader(path, DatabaseInterface::singleton()->dbFile(true)+"-wal").writeEntryToTempFiles([&](float p){ progressCallback(1.333 * p); });
	//ArchiveReader(path, DatabaseInterface::singleton()->dbFile(true)+"-shm").writeEntryToTempFiles([&](float p){ progressCallback(2.333 * p); });
    ArchiveReader(path, DatabaseInterface::singleton()->dbFile(true)).writeEntryToTempFiles([&](float p){ progressCallback(33.333 * p); });
	
	DataSetPackage::pkg()->loadWorkspace([&](float p){ progressCallback(33.333 + 33.333 * p); });

	if(resultXmlCompare::compareResults::theOne()->testMode())
	{
		//Read the results from when the JASP file was saved and store them in compareResults field

		ArchiveReader	resultsEntry	= ArchiveReader(path, "index.html");
		int				errorCode		= 0;
		std::string		html			= resultsEntry.readAllData(sizeof(char), errorCode);

		if (errorCode != 0)
			throw LoaderException("Could not read result from 'index.html' in JASP archive.");

		resultXmlCompare::compareResults::theOne()->setOriginalResult(QString::fromStdString(html));
	}
}

void JASPImporter::loadJASPArchive(const std::string &path, std::function<void(int)> progressCallback)
{
	JASPTIMER_SCOPE(JASPImporter::loadJASPArchive_1_00 read analyses.json);
	Json::Value analysesData;

	if (ArchiveReader::parseJsonEntry(analysesData, path, "analyses.json", false))
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

JASPImporter::Compatibility JASPImporter::isCompatible()
{
	if ((DataSetPackage::pkg()->jaspVersion() < JASPImporter::minJaspVersion) || (DataSetPackage::pkg()->archiveVersion().major() > JASPExporter::jaspArchiveVersion.major()) )
		return Compatibility::NotCompatible;

	if (DataSetPackage::pkg()->archiveVersion().minor()		> JASPExporter::jaspArchiveVersion.minor() )
		return Compatibility::Limited;

	return Compatibility::Compatible;
}



