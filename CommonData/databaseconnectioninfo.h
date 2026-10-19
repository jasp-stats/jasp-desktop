#ifndef DATABASECONNECTIONINFO_H
#define DATABASECONNECTIONINFO_H

#include "utilenums.h"
#include <QString>
#include <json/json.h>
#include <QSqlQuery>
#include <QTimer>

class DatabaseConnectionInfo : public QObject
{
	Q_OBJECT
	Q_PROPERTY(bool synching	READ synching				NOTIFY synchingChanged)
	
public:
	DatabaseConnectionInfo(QObject * parent = nullptr)								:	QObject(parent) { init(); }
	DatabaseConnectionInfo(const Json::Value & json, QObject * parent = nullptr)	:	QObject(parent) { init(); fromJson(json); }
	
	void		init();

	void		fromJson(const Json::Value & json);
	Json::Value	toJson(bool forJaspFile = false) const;

	bool		connect()	const;
	bool		connected()	const;
	void		close()		const;
	
	bool		startSynching(bool syncImmediately = true);
	void		stopSynching();
	bool		synching() const { return _syncher.isActive(); }
	
	QString		lastError() const;
	QSqlQuery	runQuery()	const;	

signals:
	void		synchingIntervalPassed();
	void		synchingChanged();
	QString		askPassword(	QString title, QString message);
	bool		showYesNo(		QString title, QString message);
	void		showWarning(	QString title, QString message);
	
public:
	DbType  _dbType			= DbType::NOTCHOSEN;
	QString _username		= "",
			_password		= "",
			_database		= "",
			_hostname		= "",
			_query			= "";
	int		_port			= 0,
			_interval		= 0;
	bool	_rememberMe		= false,
			_hadPassword	= false;
	QTimer	_syncher;

	//Unique QSqlDatabase connection name so that concurrent connections (e.g. two datasets syncing
	//to different databases at once) don't fight over the single unnamed default connection.
	mutable	QString _connectionName = "";

};

#endif // DATABASECONNECTIONINFO_H
