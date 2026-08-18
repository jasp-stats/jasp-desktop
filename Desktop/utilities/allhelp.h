#ifndef ALLHELP_H
#define ALLHELP_H

#include <QObject>


class AllHelp : public QObject
{
	Q_OBJECT

	Q_PROPERTY(QString	Database	READ Database	NOTIFY helpChanged)
	Q_PROPERTY(QString	PrefsAI	READ PrefsAI	NOTIFY helpChanged)
	Q_PROPERTY(QString	PrefsAdvanced	READ PrefsAdvanced	NOTIFY helpChanged)
	Q_PROPERTY(QString	PrefsData	READ PrefsData	NOTIFY helpChanged)
	Q_PROPERTY(QString	PrefsResults	READ PrefsResults	NOTIFY helpChanged)
	Q_PROPERTY(QString	PrefsUI	READ PrefsUI	NOTIFY helpChanged)
	Q_PROPERTY(QString	computedcolumns	READ computedcolumns	NOTIFY helpChanged)
	Q_PROPERTY(QString	easyfilterconstructor	READ easyfilterconstructor	NOTIFY helpChanged)
	Q_PROPERTY(QString	plotediting	READ plotediting	NOTIFY helpChanged)
	Q_PROPERTY(QString	rfilterconstructor	READ rfilterconstructor	NOTIFY helpChanged)
	Q_PROPERTY(QString	variableslabeleditorhelp	READ variableslabeleditorhelp	NOTIFY helpChanged)
	

public:
	AllHelp(QObject * parent) : QObject(parent) {};

	QString Database();
	QString PrefsAI();
	QString PrefsAdvanced();
	QString PrefsData();
	QString PrefsResults();
	QString PrefsUI();
	QString computedcolumns();
	QString easyfilterconstructor();
	QString plotediting();
	QString rfilterconstructor();
	QString variableslabeleditorhelp();
	

signals:
	void helpChanged();
};


#endif // ALLHELP_H
