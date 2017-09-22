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

#include "spssimporter_test.h"
#include <boost/lexical_cast.hpp>
#include <algorithm>
#include "desktoputils.h"


using namespace std;
using namespace boost::filesystem;

void SPSSImporterTest::init()
{
	fe_spss = new FileEvent();
	asl_spss = new AsyncLoader();
	fe_csv = new FileEvent();
	asl_csv = new AsyncLoader();
}


void SPSSImporterTest::cleanup()
{
	fe_spss->~FileEvent();
	asl_spss->~AsyncLoader();
	fe_csv->~FileEvent();
	asl_csv->~AsyncLoader();
}


void SPSSImporterTest::spssTester_data()
{
	QTest::addColumn<QString>("filename");

	_spss_path = current_path();
  #ifdef __WIN32__
	_spss_path = _spss_path.parent_path();
  #endif
	_spss_path /= TESTFILE_FOLDER;
	_spss_path /= "spssimporter_test";
	_csv_path = _spss_path;
	_spss_path /= "spss_files";
	_csv_path /= "csv_files";

#ifndef QT_NO_DEBUG
	cout << "SPSS importer SPSS test file(s) :" << _spss_path.string() << endl;
	cout << "SPSS importer CSV test file(s) :" << _csv_path.string() << endl;
#endif

	//add files to be tested in a folder "Resources/TestFiles/spssimporter_test/spss_files"
	for (auto i = directory_iterator(_spss_path.path()); i != directory_iterator(); i++)
	{
		if (!is_directory(i->path())) //we eliminate directories
		{
			QTest::newRow("spss file test") << toQStr(i->path().filename().string());
		}
	}
}


void SPSSImporterTest::spssTester()
{
	QFETCH(QString, filename);
	qDebug() << "File: " << filename;

	//spss file open
	path spss_file = _spss_path.path();
	spss_file.append(filename.toStdWString());

#ifndef QT_NO_DEBUG
	qDebug() << "SPSS Tester: Trying for .SAV file: " << spss_file.string().c_str();
#endif

	DataSetPackage *ds_spss = new DataSetPackage();
	fe_spss->setPath(QString::fromStdWString(spss_file.wstring()));
	asl_spss->loadTask(fe_spss, ds_spss);		  //load the spss file
	asl_spss->_thread.quit();

	struct fileContent fc_spss;
	copyToStructure(ds_spss, &fc_spss);			//copy contents of ds_spss to file contents structure
	SharedMemory::deleteDataSet(ds_spss->dataSet); //clear shared memory
	ds_spss->~DataSetPackage();

	//csv file open
	QString csvFile = filename;
	csvFile.replace(filename.size()-3, 3, "csv");
	path csv_file = _csv_path.path();
	csv_file.append(csvFile.toStdWString());

#ifndef QT_NO_DEBUG
	qDebug() << "SPSS Tester: Trying for .CSV file: " << csv_file.string().c_str();
#endif

	DataSetPackage *ds_csv = new DataSetPackage();
	fe_csv->setPath(QString::fromStdWString(csv_file.wstring()));
	asl_csv->loadTask(fe_csv, ds_csv);			 //load the corresponding csv file - this is the expected output
	asl_csv->_thread.quit();

	struct fileContent fc_csv;
	copyToStructure(ds_csv, &fc_csv);			  //copy contents of ds_csv to fc_csv structure
	SharedMemory::deleteDataSet(ds_csv->dataSet);  //clear the shared memory
	ds_csv->~DataSetPackage();

	QVERIFY(checkIfEqual(&fc_spss, &fc_csv));	  // end of test
}


/* copy from the DataSetPackage to fileContents structure - required since dataset is deleted in the sharedmemory  */
void SPSSImporterTest::copyToStructure(DataSetPackage *dsPackage, struct fileContent *fc)
{
	fc->columns = dsPackage->dataSet->columnCount();//copy column count
	fc->rows = dsPackage->dataSet->rowCount();	  //copy row count

	//copy header names
	std::vector<std::string> headerNames;
	for(int i=0; i<fc->columns; ++i)
	{
		headerNames.push_back(dsPackage->dataSet->column(i).name());
	}
	fc->headers = headerNames;

	//copy data
	std::vector< std::vector<std::string> > fileRows;
	for(int j=0; j<fc->rows; ++j)
	{
		std::vector<std::string> tempRow;
		for(int i=0; i<fc->columns; ++i)
		{
			tempRow.push_back(dsPackage->dataSet->column(i)[j]);
		}
		fileRows.push_back(tempRow);
		tempRow.clear();
	}

	fc->data = fileRows;
	return;
}


/* checks if data read from spss file is same as in corresponding csv file */
bool SPSSImporterTest::checkIfEqual(struct fileContent *fc1, struct fileContent *fc2)
{
	if(fc1->columns != fc2->columns)
	{
		qDebug() << "Column size mismatch: " << QString::number(fc1->columns) << " " << QString::number(fc2->columns);
		return false;
	}

	if(fc1->rows != fc2->rows)
	{
		qDebug() << "Row size mismatch: " << QString::number(fc1->rows) << " " << QString::number(fc2->rows);
		return false;
	}

	for(int i=0; i<fc2->columns; ++i)
	{
//		if(fc1->headers[i] != fc2->headers[i])
//		{
			// qDebug() << "Warning: Header name mismatch: " << toQStr(fc1->headers[i]) << " " << toQStr(fc2->headers[i]);
			//return false;
//		}

		for(int j=0; j<fc2->rows; ++j)
		{
			std::string str1 = fc1->data[j][i];
			std::string str2 = fc2->data[j][i];
			if(str1 != str2)
			{
				bool success = false;
				try
				{
					int v1 = boost::lexical_cast<int>(str1);
					int v2 = boost::lexical_cast<int>(str2);
					if (v1 == v2) success = true;
				}
				catch (...)
				{
					// Remove ""
					str1.erase(std::remove(str1.begin(), str1.end(), '"'), str1.end());
					str2.erase(std::remove(str2.begin(), str2.end(), '"'), str2.end());
					if (str1 == str2) success = true;
				}
				if (!success)
				{
					qDebug() << "Data mismatch at row: " << QString::number(j+1) << " and column: " << toQStr(fc1->headers[i]) << " (number " << QString::number(i) << ")";
					qDebug() << "CSV: " << toQStr(str2);
					qDebug() << "SPSS: " << toQStr(str1);
					return false;
				}
			}
		}
	}

	return true;
}
