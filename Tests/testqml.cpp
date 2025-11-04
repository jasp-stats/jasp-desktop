#include "testqml.h"
#include <QQmlEngine>
#include <QQmlContext>
#include "jasptheme.h"
#include "preferencesmodelbase.h"
#include "utilities/qmlutils.h"

TestQml::TestQml(QObject *parent)
	: QObject{parent}
{

}

void TestQml::applicationAvailable()
{
	// Initialization that only requires the QGuiApplication object to be available
}

void TestQml::qmlEngineAvailable(QQmlEngine *engine)
{
	// Initialization requiring the QQmlEngine to be constructed
	engine->rootContext()->setContextProperty("myContextProperty", QVariant(true));

	static QStringList originalImportPaths = engine->importPathList();

	QmlUtils::setGlobalPropertiesInQMLContext(engine->rootContext());

	QStringList newImportPaths = originalImportPaths;

	newImportPaths.append(":/jasp-stats.org/imports");
	newImportPaths.append("qrc:///components");

	engine->setImportPathList(newImportPaths);

	_theme = new JaspTheme();
	_prefs = new PreferencesModelBase(engine);

	engine->rootContext()->setContextProperty("jaspTheme",			_theme);
	engine->rootContext()->setContextProperty("preferencesModel",	_prefs);

}

void TestQml::cleanupTestCase()
{
}


QUICK_TEST_MAIN_WITH_SETUP(qmltest, TestQml);

#include "testqml.moc"
