#include "testqml.h"
#include <QQmlEngine>
#include "tempfiles.h"
#include "processinfo.h"
#include "datasetprovider.h"
#include "utilities/qmlutils.h"
#include "utilities/settings.h"
#include "qquick/scriptconstructorview.h"

TestQml::TestQml(QObject *parent)
	: QObject{parent}
{
	TempFiles::init(ProcessInfo::currentPID());
	TempFiles::clearSessionDir();
	
	Settings::informSettingsThatThisIsATest();

	DataSetProvider* prov = DataSetProvider::getProvider(false, true, parent);

	std::map<std::string, stringvec > dataSet;
	dataSet["TestInts"] = {"1", "2", "3", "4", "5"};
	dataSet["TestLetters"] = {"A", "B", "C", "D", "E"};
	dataSet["TestDoubles"] = {".2", "1.2", "0.6", "3.2", "1"};
	dataSet["TestNominal"] = {"1", "1", "1", "2", "2"};

	prov->loadDataSet(dataSet);
}

void TestQml::applicationAvailable()
{
	// Initialization that only requires the QGuiApplication object to be available
}

void TestQml::qmlEngineAvailable(QQmlEngine *engine)
{
	// Initialization requiring the QQmlEngine to be constructed
	QmlUtils::setupQMLEngine(engine);

	qmlRegisterType<ScriptConstructorView>("JASP", 1, 0, "ScriptConstructor");
}

void TestQml::cleanupTestCase()
{
}

QUICK_TEST_MAIN_WITH_SETUP(qmltest, TestQml);

#include "testqml.moc"
