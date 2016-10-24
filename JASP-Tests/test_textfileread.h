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

#ifndef TESTTEXTFILEREAD_H
#define TESTTEXTFILEREAD_H

#pragma once

#include <sstream>
#include <fstream>
#include <vector>
#include <string>
#include <utility>
#include <boost/filesystem.hpp>

#include "AutomatedTests.h"
#include "sharedmemory.h"
#include "datasetpackage.h"
#include "importers/csvimporter.h"


class TestTextFileRead : public QObject
{
	Q_OBJECT

public:

	struct fileContent
	{
		int columns;
		int rows;
		std::vector <std::string> headers;
		std::vector< std::vector<std::string> > data;
	};

	DataSetPackage *dsp;

	std::pair<int, std::string> readDataFromFile(std::string, struct fileContent*);
	std::pair<bool, std::string> checkIfEqual(struct fileContent *);
	void emptyHandler(std::string, int);

private slots:
	void initTestCase();
	void init();
	void cleanup();
	void csvImporterTest_data();
	void csvImporterTest();
};

DECLARE_TEST(TestTextFileRead)

#endif // TESTTEXTFILEREAD_H
