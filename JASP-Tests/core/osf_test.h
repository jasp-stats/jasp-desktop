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

#ifndef OSFTEST_H
#define OSFTEST_H

#pragma once
#include <sstream>
#define private public

#include <QEventLoop>
#include <QSignalSpy>
#include <QMetaType>
#include <fstream>
#include "AutomatedTests.h"
#include "backstage/fsbrowser.h"
#include "backstage/fsbmosf.h"
#include "backstage/fsbmodel.h"
#include "backstage/fsentrywidget.h"
#include "backstage/fsentry.h"
#include "backstage/backstageosf.h"
#include "sharedmemory.h"
#include "onlinedatamanager.h"
#include "fileevent.h"
#include "asyncloader.h"
#include "datasetpackage.h"


class OSFTest : public QObject
{
    Q_OBJECT

public:
    FSBrowser *fs;
    FSBMOSF *_model;
    OnlineDataManager *_odm;

    bool authenticationTest(QString, QString);
    void waitTillExists(QButtonGroup *);

private slots:
    void initTestCase();
    void init();
    void cleanup();
    void cleanupTestCase();
    void loginAuthenticationTest_data();
    void loginAuthenticationTest();
    void fileListTest();
};

DECLARE_TEST(OSFTest)

#endif // OSFTEST_H
