#ifndef TESTQML_H
#define TESTQML_H

#include <QtQuickTest>

class PreferencesModelBase;
class DataSetProvider;
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
};

#endif // TESTQML_H
