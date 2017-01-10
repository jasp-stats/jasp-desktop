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

#include "osf_test.h"
#include "tempfiles.h"
#include "processinfo.h"


void OSFTest::initTestCase()
{

}

void OSFTest::init()
{
  fs = new FSBrowser();
  _model = new FSBMOSF();
  _odm = new OnlineDataManager(this);
}

void OSFTest::cleanup()
{
  fs->~FSBrowser();
  _model->~FSBMOSF();
  _odm->~OnlineDataManager();
}


void OSFTest::cleanupTestCase()
{

}


/* data for the loginAuthenticationTest function */
void OSFTest::loginAuthenticationTest_data() 
{
  QTest::addColumn<QString>("username");
  QTest::addColumn<QString>("password");
  QTest::addColumn<bool>("result");

  QTest::newRow("1") << "jasp.tester@gmail.com" << "testerJASP" << true;
  QTest::newRow("2") << "Hello.com" << "123456qwerty" << false;
  QTest::newRow("3") << "jasp.tester@gmail.com" <<  "12345qwerty" << false;
  QTest::newRow("4") << "osftester1@yopmail.com" <<  "!@#$%^" << true;
  QTest::newRow("5") << "osftester2@yopmail.com" <<  "" << false;
  QTest::newRow("6") << "" <<  "12345qwerty" << false;
  QTest::newRow("7") << "" <<  "" << false;
}


void OSFTest::loginAuthenticationTest()
{
  QFETCH(QString, username);
  QFETCH(QString, password);
  QFETCH(bool, result);

  QCOMPARE(authenticationTest(username, password), result);
}

bool OSFTest::authenticationTest(QString username, QString password)
{
  _model->setOnlineDataManager(_odm);

  fs->setFSModel(_model);

//  bool wasBlocked = fs->_model->blockSignals(true);

//  bool wasBlocked1 = fs->_model->signalsBlocked();

  fs->setFSModel(_model);

  fs->loginRequested(username, password);

  if(_model->isAuthenticated())
  {
    return true;
  }

  return false;
}

void OSFTest::fileListTest()
{
  tempfiles_init(ProcessInfo::currentPID()); //set the root path(directory)
  qRegisterMetaType<FileEvent *>();//register the datatype FileEvent
  BackstageOSF *bosf = new BackstageOSF(); //initialize BackstageOSF
  DataSetPackage *dsf = new DataSetPackage();
  AsyncLoader *asf = new AsyncLoader();

  asf->setOnlineDataManager(_odm);
  bosf->setOnlineDataManager(_odm);
  bosf->_fsBrowser->loginRequested("jasp.tester@gmail.com", "testerJASP");

  QSignalSpy spy(bosf, SIGNAL(dataSetIORequest(FileEvent *))); //spy for dataSetIORequest

  if(bosf->_model->isAuthenticated())
  {
    bosf->_model->refresh();

    QButtonGroup *buttonGroup = bosf->_fsBrowser->_buttonGroup;

    waitTillExists(buttonGroup);

    if(!(buttonGroup->button(0)))
    {
      QVERIFY2(false, "Button doesn't exist");
    }

    // simulate (GUI) mouse double clicks
	  QTest::mouseDClick(buttonGroup->button(0), Qt::LeftButton,Qt::NoModifier, QPoint(), 50);
    waitTillExists(buttonGroup);
    QTest::mouseDClick(buttonGroup->button(0), Qt::LeftButton,Qt::NoModifier, QPoint(), 50);    
    waitTillExists(buttonGroup);

    FSEntryWidget *entry = qobject_cast<FSEntryWidget*>(buttonGroup->button(0));
    qDebug() << "File selected - " << entry->_entry.name << "\n"; //jasp_tester.jasp
    QTest::mouseDClick(buttonGroup->button(0), Qt::LeftButton,Qt::NoModifier, QPoint(), 50);

    FileEvent *fevent = qvariant_cast<FileEvent *>(spy.at(0).at(0)); //filevent parameter of the dataSetIORequest event

    fevent->setComplete(false, "initialize the set");
    QSignalSpy spy2(fevent, SIGNAL(completed(FileEvent *))); //completed signal is emitted when dataset load is complete

    asf->loadTask(fevent, dsf);

    while(spy2.count() != 1)
    {
      QTest::qWait(250);
    }

    QVERIFY(fevent->_success); //_success is true only if data is loaded
    // destroy all the objects created and delete the dataSet from the shared memory
    SharedMemory::deleteDataSet(dsf->dataSet);
  }
}


/* waits until the button is created */ 
void OSFTest::waitTillExists(QButtonGroup *buttonGroup)
{
  QTest::qWait(2000);

  while(!(buttonGroup->button(0)))
  {
    QTest::qWait(250);
  }
}
