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

#ifndef SPSSIMPORTERTEST_H
#define SPSSIMPORTERTEST_H

#pragma once
#include <sstream>
#define private public

#include <vector>
#include <string>
#include <boost/filesystem.hpp>
#include "AutomatedTests.h"
#include "asyncloader.h"
#include "sharedmemory.h"
#include "fileevent.h"
#include "mainwindow.h"
#include "datasetpackage.h"


class SPSSImporterTest : public QObject
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

  FileEvent *fe_spss, *fe_csv;
  AsyncLoader *asl_spss, *asl_csv;

  bool checkIfEqual(struct fileContent*, struct fileContent*);
  void copyToStructure(DataSetPackage*, struct fileContent*);

private slots:
    void init();
    void cleanup();
    void spssTester();
    void spssTester_data();
};


DECLARE_TEST(SPSSImporterTest)

#endif // SPSSIMPORTERTEST_H
