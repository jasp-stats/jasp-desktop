//
// Copyright (C) 2013-2018 University of Amsterdam
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

#include "odsimporter.h"
#include "sharedmemory.h"

#include "ods/odsxmlmanifesthandler.h"
#include "ods/odsxmlcontentshandler.h"
#include "archivereader.h"

#include <QXmlInputSource>

#include "timers.h"

namespace ods
{

// Implmemtation of Inporter base class.
ImportDataSet* ODSImporter::loadFile(const std::string &locator, boost::function<void(int)> progressCallback)
{
	JASPTIMER_RESUME(ODSImporter::loadFile);

	// Create new data set.
	ODSImportDataSet * result = new ODSImportDataSet(this);

	// Check mnaifest for the contents file.
	progressCallback(0); //"Reading ODS manifest.",
	readManifest(locator, result);

	// Read the sheet contents.
	progressCallback(33); // "Reading ODS contents.",
	readContents(locator, result);

	// Do post load processing:
	progressCallback(60); //"Processing.",
	result->postLoadProcess();

	// Build the dictionary for sync.
	result->buildDictionary();

	JASPTIMER_STOP(ODSImporter::loadFile);

	return result;
}

void ODSImporter::readManifest(const std::string &path, ODSImportDataSet *dataset)
{

	QXmlInputSource src;
	{
		// Get the data file proper from the ODS manifest file.
		ArchiveReader manifest(path, ODSImportDataSet::manifestPath);
		std::string tmp;
		int errorCode = 0;
		if (((tmp = manifest.readAllData(4096, errorCode)).size() == 0) || (errorCode < 0))
			throw std::runtime_error("Error reading manifest in ODS.");
		src.setData(QString::fromStdString(tmp));
		manifest.close();
	}

	{
		XmlManifestHandler * manHandler = new XmlManifestHandler(dataset);
		QXmlSimpleReader reader;
		reader.setContentHandler(manHandler);
		reader.setErrorHandler(manHandler);
		reader.parse(src);
	}
}

void ODSImporter::readContents(const std::string &path, ODSImportDataSet *dataset)
{

	ArchiveReader contents(path, dataset->getContentFilename());

	QXmlInputSource src;
	{
		std::string tmp;
		int errorCode = 0;
		if (((tmp = contents.readAllData(4096, errorCode)).size() == 0) || (errorCode < 0))
			throw std::runtime_error("Error reading contents in ODS.");
		src.setData(QString::fromStdString(tmp));
	}

	{
		XmlContentsHandler * contentsHandler = new XmlContentsHandler(dataset);
		QXmlSimpleReader reader;
		reader.setContentHandler(contentsHandler);
		reader.setErrorHandler(contentsHandler);
		reader.parse(src);
	}

	contents.close();
}

}
