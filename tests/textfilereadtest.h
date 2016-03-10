#ifndef TEXTFILEREADTEST_H
#define TEXTFILEREADTEST_H

#pragma once
#define private public

#include <QSignalSpy>
#include <QSignalBlocker>
#include <fstream>
#include <vector>
#include <sstream>
#include <string>
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

  void readFromFile(std::string, struct fileContent*);

private slots:
    void initTestCase();
    void cleanupTestCase();
    void init();
    void cleanup();
    void asyncloaderTester1();
    void asyncloaderTester2();
};


DECLARE_TEST(TextFileReadTest)

#endif // TEXTFILEREADTEST_H
