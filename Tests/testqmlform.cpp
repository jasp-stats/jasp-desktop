#include "testqmlform.h"
#include <QQmlEngine>
#include <QQmlContext>
#include "jasptheme.h"
#include "preferencesmodelbase.h"

TestForm::TestForm(QObject *parent)
	: QObject{parent}
{

}

void TestForm::applicationAvailable()
{
	// Initialization that only requires the QGuiApplication object to be available
}

void TestForm::qmlEngineAvailable(QQmlEngine *engine)
{
	// Initialization requiring the QQmlEngine to be constructed
	engine->rootContext()->setContextProperty("myContextProperty", QVariant(true));

	static QStringList originalImportPaths = engine->importPathList();

	QStringList newImportPaths = originalImportPaths;

	newImportPaths.append(":/jasp-stats.org/imports");
	newImportPaths.append("qrc:///components");

	engine->setImportPathList(newImportPaths);

	_theme = new JaspTheme();
	_prefs = new PreferencesModelBase(engine);

	engine->rootContext()->setContextProperty("jaspTheme",			_theme);
	engine->rootContext()->setContextProperty("preferencesModel",	_prefs);

}

void TestForm::cleanupTestCase()
{
}


QUICK_TEST_MAIN_WITH_SETUP(qmlformtest, TestForm);

#include "testqmlform.moc"
