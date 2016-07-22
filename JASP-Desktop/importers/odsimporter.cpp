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


Data * ODSImporter::_dta = 0;

void ODSImporter::loadDataSet(
	DataSetPackage *packageData,
	const string &locator,
	boost::function<void (const string &, int)> progress
)
{
	packageData->isArchive = false;	// Spreadsheet files are never JASP archives.

	// Build our data collection object.
	try
	{
		if (_dta != 0)
			delete _dta;
		_dta = new Data();

		// Check mnaifest for the contents file.
		progress("Reading ODS manifest.", 0);
		readManifest(packageData, locator);

		// Read the sheet contents.
		progress("Reading ODS contents.", 2);
		readContents(locator);

		// Process and pass to app.
		progress("Reading ODS contents.", 80);
		vector<Data::JaspColumn> colMeta = _dta->process();

		// Importing
		// Init the data set
		bool success = false;
		packageData->dataSet = SharedMemory::createDataSet();
		do
		{
			try
			{
				success = true;
				packageData->dataSet->setColumnCount(_dta->sheet().numColumns());
				packageData->dataSet->setRowCount(_dta->sheet().numRows());
			}
			catch (boost::interprocess::bad_alloc &e)
			{
				try {

					packageData->dataSet = SharedMemory::enlargeDataSet(packageData->dataSet);
					success = false;
				}
				catch (std::exception &e)
				{
					throw runtime_error("Out of memory: This data set is too large for your computer's available memory.");
				}
			}
			catch (std::exception &e)
			{
				cout << "n " << e.what() << "\n";
				cout.flush();
			}
			catch (...)
			{
				cout << "something else\n ";
				cout.flush();
			}
		}
		while (success == false);

		// Got enough memory, set up the colums and data.
		const Data::Sheet &sheet = _dta->sheet();
		for (int columnNumber = 0; columnNumber < sheet.numColumns(); columnNumber++)
		{
			Column &jaspCol = packageData->dataSet->column(columnNumber);
			jaspCol.labels().clear();
			jaspCol.setName(colMeta[columnNumber].lable());
			jaspCol.setColumnType(colMeta[columnNumber].type());

			const Data::SheetColumn &sheetCol = sheet[columnNumber];

			switch(colMeta[columnNumber].type())
			{
			case Column::ColumnTypeNominalText:
				// insert all the strings as labels.
				for (int i = 0; i < sheetCol.numberLabels(); i++)
					jaspCol.labels().add(sheetCol.labelAt(i));
				// Drop through!
			case Column::ColumnTypeNominal:
			case Column::ColumnTypeOrdinal:
			{
				// Insert integer values, inserting empty where the column has no rows.
				Column::Ints::iterator addIter = jaspCol.AsInts.begin();
				for (int rowCnt = 1; rowCnt <= sheet.maxRow(); rowCnt++)
				{
					if ((rowCnt < sheetCol.minRow()) || (rowCnt > sheetCol.maxRow()))
						*addIter = Data::SheetCellLong::EmptyInt;
					else
						*addIter = _dta->sheet()[columnNumber].valueAsInt(rowCnt);\
					addIter++;
				}
			}
				break;

			case Column::ColumnTypeScale:
			{
				// Insert double values, inserting empty where the column has no rows.
				Column::Doubles::iterator addIter = jaspCol.AsDoubles.begin();
				for (int rowCnt = 1; rowCnt <= sheet.maxRow(); rowCnt++)
				{
					if ((rowCnt < sheetCol.minRow()) || (rowCnt > sheetCol.maxRow()))
						*addIter = Data::SheetCellLong::EmptyDouble;
					else
						*addIter = _dta->sheet()[columnNumber].valueAsDouble(rowCnt);
					addIter++;
				}
			}
			case Column::ColumnTypeUnknown:
				break;
			}
		}
	}
	catch(...)
	{
		delete _dta;
		_dta = 0;
		throw;
	}
}


void ODSImporter::readManifest(DataSetPackage *packageData, const string &path)
{

	QXmlInputSource src;
	{
		FileReader manifest(path, _dta->manifestPath);
		QString tmp;
		int errorCode = 0;
		if (((tmp = manifest.readAllData(errorCode)).size() == 0) || (errorCode < 0))
			throw runtime_error("Error reading manifest in ODS.");
		src.setData(tmp);
		manifest.close();
	}

	{
		XmlManifestHandler * manHandler = new XmlManifestHandler(_dta);
		QXmlSimpleReader reader;
		reader.setContentHandler(manHandler);
		reader.setErrorHandler(manHandler);
		reader.parse(src);
	}
}

void ODSImporter::readContents(const string &path)
{

	FileReader contents(path, _dta->getContentFilename().toStdString());

	QXmlInputSource src;
	{
		QString tmp;
		int errorCode = 0;
		if (((tmp = contents.readAllData(errorCode)).size() == 0) || (errorCode < 0))
			throw runtime_error("Error reading contents in ODS.");
		src.setData(tmp);
	}

	{
		XmlContentsHandler * contentsHandler = new XmlContentsHandler(_dta);
		QXmlSimpleReader reader;
		reader.setContentHandler(contentsHandler);
		reader.setErrorHandler(contentsHandler);
		reader.parse(src);
	}

	contents.close();
}

