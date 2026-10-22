#include "testqml.h"
#include <QQmlEngine>
#include "tempfiles.h"
#include "processinfo.h"
#include "dirs.h"
#include <QDir>
#include "datasetprovider.h"
#include "utilities/qmlutils.h"
#include "utilities/settings.h"
#include "qquick/scriptconstructorview.h"

TestQml::TestQml(QObject *parent)
	: QObject{parent}
{
	//Keep the session dir out of the CWD, see TestAll::initTestCase.
	Dirs::setLocalAppdataDir(QDir::tempPath().toStdString() + "/jasp-test-runs");
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

	//A second dataset with its own filters and its own columns: enough for per-list dataset
	//selection (VariablesListBase::dataSetSelectionOption, see tst_dataSetSelectionVariablesList.qml)
	//to select between, and for tests to prove two selecting lists look at *different* data:
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
			second->createFilter("ReopenMe", true);
			ws->setShownDataSet(first);
		}

		//Then reopen the database the way File > Open does: the workspace is rebuilt from the
		//stored rows and every test runs against a *reopened file* - the state the eager filter
		//materialisation of DataSet::dbLoad exists for. 'ReopenMe' reaches
		//the dataset/filter dropdown only because dbLoad materialised the stored row; the
		//reopen tests in tst_dataSetSelectionVariablesList.qml assert exactly that.
		prov->reloadWorkspace();
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

	// The multi-dataset fixture workspace (dataset 1 + "Second"), the same object the app
	// exposes as a context property for the data editors. The qualified-column-ref editor
	// tests (tst_qualifiedColumnRefs.qml) drive the D7.1/D7.2 picker/validator components
	// against it; the components take the workspace as a property, exactly like the
	// FilterWindow/ComputeColumnWindow/ComputeDataSetPanel wiring does.
	if(Workspace * ws = Workspace::singleton())
		engine->rootContext()->setContextProperty("workspace", ws);

	// Required: this harness never constructs a MainWindow, and the production
	// registration happens in the MainWindow constructor (not at static init).
	qmlRegisterType<ScriptConstructorView>("JASP", 1, 0, "ScriptConstructor");
	qmlRegisterUncreatableMetaObject(ScriptConstructorEnums::staticMetaObject, "JASP", 1, 0, "ScriptConstructorMode", "enums only");
}

void TestQml::cleanupTestCase()
{
}

QUICK_TEST_MAIN_WITH_SETUP(qmltest, TestQml);

#include "testqml.moc"
