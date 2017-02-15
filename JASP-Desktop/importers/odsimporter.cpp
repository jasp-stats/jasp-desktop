//
// Copyright (C) 2013-2016 University of Amsterdam
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
#include "filereader.h"

#include <QXmlInputSource>



using namespace std;
using namespace boost;
using namespace ods;


//Data * ODSImporter::_dta = 0;

ODSImporter::ODSImporter(DataSetPackage *packageData)
	: Importer(packageData)
{
	// Spreadsheet files are never JASP archives.
	packageData->isArchive = false;
}

ODSImporter::~ODSImporter()
{
}




// Implmemtation of Inporter base class.
ImportDataSet* ODSImporter::loadFile(const string &locator, boost::function<void(const string &, int)> progressCallback)
{
	// Create new data set.
	ODSImportDataSet * result = new ODSImportDataSet();

	// Check mnaifest for the contents file.
	progressCallback("Reading ODS manifest.", 0);
	readManifest(locator, result);

	// Read the sheet contents.
	progressCallback("Reading ODS contents.", 33);
	readContents(locator, result);

	// Do post load processing:
	progressCallback("Processing.", 60);
	result->postLoadProcess();

	// Build the dictionary for sync.
	result->buildDictionary();

	return result;
}

void ODSImporter::fillSharedMemoryColumn(ImportColumn *importColumn, Column &column)
{
	const ODSImportColumn &impCol = static_cast<const ODSImportColumn &> (*importColumn);
	column.setColumnType( impCol.getJASPColumnType() );

	// Pass the message on to the columns, and find the maximum rows/cases.
	impCol.fillSharedMemoryColumn(column);

}

void ODSImporter::readManifest(const string &path, ODSImportDataSet *dataset)
{

	QXmlInputSource src;
	{
		// Get the data file proper from the ODS manifest file.
		FileReader manifest(path, ODSImportDataSet::manifestPath);
		QString tmp;
		int errorCode = 0;
		if (((tmp = manifest.readAllData(errorCode)).size() == 0) || (errorCode < 0))
			throw runtime_error("Error reading manifest in ODS.");
		src.setData(tmp);
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

void ODSImporter::readContents(const string &path, ODSImportDataSet *dataset)
{

	FileReader contents(path, dataset->getContentFilename());

	QXmlInputSource src;
	{
		QString tmp;
		int errorCode = 0;
		if (((tmp = contents.readAllData(errorCode)).size() == 0) || (errorCode < 0))
			throw runtime_error("Error reading contents in ODS.");
		src.setData(tmp);
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

