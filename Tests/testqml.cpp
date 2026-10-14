#include "testqml.h"
#include <QQmlEngine>
#include <QQmlComponent>
#include <QQmlContext>
#include <QQmlPropertyMap>
#include <QFontDatabase>
#include "tempfiles.h"
#include "processinfo.h"
#include "dirs.h"
#include <QDir>
#include "datasetprovider.h"
#include "utilities/qmlutils.h"
#include "utilities/settings.h"
#include "qquick/scriptconstructorview.h"
#include "jasptheme.h"

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
}

void TestQml::applicationAvailable()
{
	// Initialization that only requires the QGuiApplication object to be available
}

void TestQml::qmlEngineAvailable(QQmlEngine *engine)
{
	// Initialization requiring the QQmlEngine to be constructed
	QmlUtils::setupQMLEngine(engine);

	// Required: this harness never constructs a MainWindow, and the production
	// registration happens in the MainWindow constructor (not at static init).
	qmlRegisterType<ScriptConstructorView>("JASP", 1, 0, "ScriptConstructor");
	qmlRegisterUncreatableMetaObject(ScriptConstructorEnums::staticMetaObject, "JASP", 1, 0, "ScriptConstructorMode", "enums only");

	// Use the theme of the application (as MainWindow does): a default constructed JaspTheme has no sizes, so the controls would get no real geometry.
	qmlRegisterType<JaspTheme>("JASP", 1, 0, "JaspTheme");
	// The theme gets its fonts from the preferences of the application, which the PreferencesModelBase used here does not have.
	QQmlContext*	themeContext	= new QQmlContext(engine->rootContext(), engine);
	QQmlPropertyMap*	themePrefs	= new QQmlPropertyMap(themeContext);
	themePrefs->insert("interfaceFont",	QFontDatabase::systemFont(QFontDatabase::GeneralFont).family());
	themePrefs->insert("codeFont",		QFontDatabase::systemFont(QFontDatabase::FixedFont).family());
	themeContext->setContextProperty("preferencesModel", themePrefs);

	QQmlComponent themeComponent(engine, QUrl("qrc:/components/JASP/Theme/Theme.qml"));
	JaspTheme* theme = qobject_cast<JaspTheme*>(themeComponent.create(themeContext));
	if (theme)
	{
		theme->setParent(engine);
		JaspTheme::setCurrentTheme(theme);
		engine->rootContext()->setContextProperty("jaspTheme", theme);
	}
	else
		qWarning() << "Could not load the JASP theme:" << themeComponent.errorString();
}

void TestQml::cleanupTestCase()
{
}

QUICK_TEST_MAIN_WITH_SETUP(qmltest, TestQml);

#include "testqml.moc"
