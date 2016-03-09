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

#include "osftest.h"

void OSFTest::initTestCase()
{
  qDebug() << "********* Beginning OSFTest**********";
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
  qDebug() << "*********  Ending OSFTest  **********";
}


/* data for the loginAuthenticationTest function */
void OSFTest::loginAuthenticationTest_data() 
{
  QTest::addColumn<QString>("username");
  QTest::addColumn<QString>("password");
  QTest::addColumn<bool>("result");

  QTest::newRow("1") << "akash.07.raj@gmail.com" << "123456qwerty" << true;
  QTest::newRow("2") << "Hello.com" << "123456qwerty" << false;
  QTest::newRow("3") << "akash.07.raj@gmail.com" <<  "12345qwerty" << false;
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

  bool wasBlocked = fs->_model->blockSignals(true);

  bool wasBlocked1 = fs->_model->signalsBlocked();

  fs->setFSModel(_model);

  fs->loginRequested(username, password);

  if(_model->isAuthenticated())
  {
    return true;
  }

  return false;
}





