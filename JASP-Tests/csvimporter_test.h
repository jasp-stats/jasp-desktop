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

#ifndef CSVIMPORTERTEST_H
#define CSVIMPORTERTEST_H

#pragma once
#include <sstream>
#define private public

#include <QSignalSpy>
#include <fstream>
#include <vector>
#include <string>
#include <boost/filesystem.hpp>
#include <iomanip>
#include <cstdio>
#include "AutomatedTests.h"
#include "asyncloader.h"
#include "sharedmemory.h"
#include "fileevent.h"
#include "mainwindow.h"
#include "datasetpackage.h"


class CSVImporterTest : public QObject
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

  FileEvent *fe;
  DataSetPackage *dsp;
  AsyncLoader *asl;
  std::vector<bool> columnIsNumeric;

  bool checkIfEqual(struct fileContent *);
  int readDataFromCSV(QString, struct fileContent*);
  std::string roundTo6Digits(double, int);
  bool checkIfNumeric(std::string);

private slots:
    void initTestCase();
    void cleanupTestCase();
    void init();
    void cleanup();
    void csvTester();
    void csvTester_data();
};


DECLARE_TEST(CSVImporterTest)

#endif // CSVIMPORTERTEST_H
