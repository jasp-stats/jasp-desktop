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





