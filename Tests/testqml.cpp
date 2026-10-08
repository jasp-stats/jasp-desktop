#include "testqml.h"
#include <QQmlEngine>
#include "tempfiles.h"
#include "processinfo.h"
#include "datasetprovider.h"
#include "utilities/qmlutils.h"
#include "utilities/settings.h"

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

	//A second dataset with its own filters and its own columns: enough for per-form dataset
	//selection (VariablesForm::dataSetSelectionOption, see tst_dataSetSelectionVariablesForm.qml)
	//to select between, and for tests to prove two forms look at *different* data:
	//"TestInts" exists in both datasets with different values, "SecondOnly" only here.
	//The first dataset is shown again right away, so all other QML tests keep looking at their data.
	if(DataSet * first = prov->dataSet())
	{
		Workspace * ws = first->workspace();

		std::map<std::string, stringvec > secondSet;
		secondSet["TestInts"]   = {"100", "200", "300"};
		secondSet["SecondOnly"] = {"x", "y", "z"};
		prov->loadDataSet(secondSet, 10, true, "Second");

		DataSet * second = ws ? ws->dataSetByTitle("Second") : nullptr;

		if(second)
		{
			second->addFilter();
			ws->setShownDataSet(first);
		}
	}
}

void TestQml::applicationAvailable()
{
	// Initialization that only requires the QGuiApplication object to be available
}

void TestQml::qmlEngineAvailable(QQmlEngine *engine)
{
	// Initialization requiring the QQmlEngine to be constructed
	QmlUtils::setupQMLEngine(engine);

}

void TestQml::cleanupTestCase()
{
}

QUICK_TEST_MAIN_WITH_SETUP(qmltest, TestQml);

#include "testqml.moc"
