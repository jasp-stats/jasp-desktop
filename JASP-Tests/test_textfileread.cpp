//
// Copyright (C) 2016 University of Amsterdam
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

#include "test_textfileread.h"


void TestTextFileRead::initTestCase() {

	bool folderPathFound;

	if (boost::filesystem::exists(TESTFILE_FOLDER "test_textfileread")) {
		folderPathFound = true;
	} else {
		folderPathFound = false;
	}

	QVERIFY2(folderPathFound, "Testfiles path not found");
}

void TestTextFileRead::init() {

	dsp = new DataSetPackage();
}

void TestTextFileRead::cleanup() {
	// Destroy all the objects created and delete the dataSet from the shared memory
	if (dsp->dataSet != NULL) {
		SharedMemory::deleteDataSet(dsp->dataSet);
	}

	dsp->~DataSetPackage();
}

void TestTextFileRead::csvImporterTest_data() {

	QTest::addColumn<QString>("filename");
	boost::filesystem::path _path(TESTFILE_FOLDER "test_textfileread");

	// Add files to be tested in a folder "Resources/TestFiles/spssimporter_test/spss_files"
	for (auto i = boost::filesystem::directory_iterator(_path); i != boost::filesystem::directory_iterator(); i++) {
		// Directories are not considered
		if (!boost::filesystem::is_directory(i->path())) {
			QTest::newRow(i->path().filename().string().c_str()) << QString::fromStdString(i->path().filename().string());
		}
	}
}

void TestTextFileRead::csvImporterTest() {

	QFETCH(QString, filename);

	// Text file open
	QString folderPath = TESTFILE_FOLDER "test_textfileread/";
	QString _path = folderPath.append(filename);

	struct fileContent fc;
	std::pair<int, std::string> error = readDataFromFile(_path.toUtf8().constData(), &fc);

	QVERIFY2(error.first == 0, error.second.c_str());

	CSVImporter::loadDataSet(dsp, _path.toStdString(), boost::bind(&TestTextFileRead::emptyHandler, this, _1, _2));

	std::pair<bool, std::string> isEqual = checkIfEqual(&fc);
	QVERIFY2(isEqual.first, isEqual.second.c_str());
}

void TestTextFileRead::emptyHandler(std::string _1, int _2) {
	// FIXME: test the calls to this function (mock the function)
	// this function should be called from loadDataSet method in CSVImporter function
}

/* checks if data read from file is same as the data stored in the shared memory */
std::pair<bool, std::string> TestTextFileRead::checkIfEqual(struct fileContent *fc) {

	// FIXME: make error messages more informative
	if (fc->columns != dsp->dataSet->columnCount()) {
		return std::make_pair(false, "Column size mismatch");
	}

	if (fc->rows != dsp->dataSet->rowCount()) {
		return std::make_pair(false, "Row size mismatch");
	}

	for (int i = 0; i < fc->columns; ++i) {

		if (fc->headers[i] != dsp->dataSet->column(i).name()) {
			return std::make_pair(false, "Column name mismatch");
		}

		for (int j = 0; j<fc->rows; ++j) {
			if (fc->data[j][i] != dsp->dataSet->column(i)[j]) {
				return std::make_pair(false, "Data mismatch");
			}
		}
	}

	return std::make_pair(true, "");
}

/* Read data from the file specified from path and store it in the struct fileContent */
std::pair<int, std::string> TestTextFileRead::readDataFromFile(std::string path, struct fileContent *fc) {

	std::ifstream infile(path.c_str());

	if (!infile.is_open()) {
		return std::make_pair(1, "File open failed");
	}

	char delimiter = '\t';
	int numRows = 0, numCols = 0;
	std::string line, currentWord;
	std::vector<std::string> tempRow;
	std::vector< std::vector<std::string> > fileRows;

	std::getline(infile, line);
	std::istringstream buffer(line);
	std::size_t found = line.find(delimiter);

	// determine the delimiter
	if (found == std::string::npos) {
	  delimiter = ' ';
	}

	while (std::getline(buffer, currentWord, delimiter)) {
		numCols++;
		tempRow.push_back(currentWord);
	}

	fc->columns = numCols;
	fc->headers = tempRow;
	buffer.clear();
	tempRow.clear();

	while (std::getline(infile, line)) {

		numRows++;
		buffer.str(line);
		int numWordsCurrent = 1;

		for (size_t i = 0; i < line.size(); ++i) {
			if (line[i] == delimiter) {
				numWordsCurrent++;
			}
		}

		for (int i = 0; i < numWordsCurrent; ++i) {
			std::getline(buffer, currentWord, delimiter);
			bool valid = false;
			// check if current word has letters/numbers
			for (size_t j = 0; j < currentWord.size(); ++j) {
				if (currentWord[j] != ' ' && currentWord[j] != '\t') {
					valid = true;
					break;
				}
			}

			if (valid) {
				tempRow.push_back(currentWord);
			} else {
				tempRow.push_back(".");
			}
		}

		// fill remaining with '.'
		for (int i = numWordsCurrent; i < numCols; ++i) {
			tempRow.push_back(".");
		}

		fileRows.push_back(tempRow);
		tempRow.clear();
		buffer.clear();
	}

	fc->rows = numRows;
	fc->data = fileRows;

	if (infile.bad()) {
		return std::make_pair(1, "File read failed");
	}

	infile.close();

	return std::make_pair(0, "");
}
