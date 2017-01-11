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

#ifndef TEXTFILEREADTEST_H
#define TEXTFILEREADTEST_H

#pragma once
#include <sstream>
#define private public

#include <QSignalSpy>
#if QT_VERSION > QT_VERSION_CHECK(5, 3, 0)
#include <QSignalBlocker>
#endif
#include <fstream>
#include <vector>
#include <string>
#include <boost/filesystem.hpp>
#include "AutomatedTests.h"
#include "asyncloader.h"
#include "sharedmemory.h"
#include "fileevent.h"
#include "mainwindow.h"
#include "datasetpackage.h"


class TextFileReadTest : public QObject
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
  bool folderPathFound;

  int readDataFromFile(std::string, struct fileContent*);
  bool checkIfEqual(struct fileContent *);

private slots:
    void initTestCase();
    void cleanupTestCase();
    void init();
    void cleanup();
    void asyncloaderTester_data();
    void asyncloaderTester();
};


DECLARE_TEST(TextFileReadTest)

#endif // TEXTFILEREADTEST_H
