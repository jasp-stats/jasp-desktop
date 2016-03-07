#ifndef OSFTEST_H
#define OSFTEST_H

#pragma once
#define private public

#include <QEventLoop>
#include <QSignalSpy>
#include <fstream>

#include "AutomatedTests.h"
#include "backstage/fsbrowser.h"
#include "backstage/fsbmosf.h"
#include "backstage/fsbmodel.h"
#include "onlinedatamanager.h"

class OSFTest : public QObject
{
    Q_OBJECT

public:
    FSBrowser *fs;
    FSBMOSF *_model;
    OnlineDataManager *_odm;

    bool authenticationTest(QString, QString);

private slots:
    void initTestCase();
    void init();
    void cleanup();
    void cleanupTestCase();
    void loginAuthenticationTest_data();
    void loginAuthenticationTest();
};

DECLARE_TEST(OSFTest)

#endif // OSFTEST_H
