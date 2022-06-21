#ifndef DATABASECONNECTIONINFO_H
#define DATABASECONNECTIONINFO_H

#include "utilenums.h"
#include <QString>
#include <json/json.h>
#include <QSqlQuery>

class DatabaseConnectionInfo
{
public:
	DatabaseConnectionInfo() {}
	DatabaseConnectionInfo(const Json::Value & json) { fromJson(json); }
	
	void		fromJson(const Json::Value & json);
	Json::Value	toJson() const;

	bool		connect()	const;
	void		close()		const;
	
	QString		lastError() const;
	QSqlQuery	runQuery()	const;
	
	
	DbType  _dbType			= DbType::NOTCHOSEN;
	QString _username		= "",
			_password		= "",
			_database		= "",
			_hostname		= "",
			_query			= "";
	int		_port			= 0,
			_interval		= 0;
	
};

#endif // DATABASECONNECTIONINFO_H
