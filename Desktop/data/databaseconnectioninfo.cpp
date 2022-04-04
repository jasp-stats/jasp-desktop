#include "databaseconnectioninfo.h"
#include "utilities/qutils.h"
#include <QSqlDatabase>
#include <QSqlError>

Json::Value DatabaseConnectionInfo::toJson() const
{
	Json::Value out = Json::objectValue;
	
	out["dbType"]	= DbTypeToString(_dbType);
	out["username"]	= fq(_username);
	out["password"]	= fq(_password);
	out["database"]	= fq(_database);
	out["hostname"]	= fq(_hostname);
	out["query"]	= fq(_query);
	out["port"]		= _port;

	return out;
}

bool DatabaseConnectionInfo::connect() const
{
	QSqlDatabase db = QSqlDatabase::addDatabase(DbTypeToQString(_dbType));

	db.setDatabaseName(	_database);
	db.setHostName(		_hostname);
	db.setUserName(		_username);
	db.setPassword(		_password);
	db.setPort(			_port);

	return db.open();
}

QString DatabaseConnectionInfo::lastError() const
{
	return QSqlDatabase::database().lastError().text();	
}

void DatabaseConnectionInfo::fromJson(const Json::Value & json)
{
	_dbType		= DbTypeFromString(	json["dbType"]		.asString() )	;
	_username	= tq(				json["username"]	.asString() )	;
	_password	= tq(				json["password"]	.asString() )	;
	_database	= tq(				json["database"]	.asString() )	;
	_hostname	= tq(				json["hostname"]	.asString() )	;
	_query		= tq(				json["query"]		.asString() )	;
	_port		=					json["port"]		.asUInt()		;

}
