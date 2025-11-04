#ifndef TESTQML_H
#define TESTQML_H

#include <QtQuickTest>

class PreferencesModelBase;
class QQmlEngine;
class JaspTheme;

class TestQml : public QObject
{
	Q_OBJECT
public:
	explicit TestQml(QObject *parent = nullptr);

public slots:
	void applicationAvailable();

	void qmlEngineAvailable(QQmlEngine *engine);

	void cleanupTestCase();

private:
	JaspTheme				* _theme = nullptr;
	PreferencesModelBase	* _prefs = nullptr;
};

#endif // TESTQML_H
