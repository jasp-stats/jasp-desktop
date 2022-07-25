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
	Json::Value	toJson(bool forJaspFile = false) const;

	bool		connect()	const;
	bool		connected()	const;
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
	bool	_rememberMe		= false,
			_hadPassword	= false;
	
};

#endif // DATABASECONNECTIONINFO_H
