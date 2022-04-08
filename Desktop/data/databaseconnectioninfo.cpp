#include "databaseconnectioninfo.h"
#include "utilities/qutils.h"
#include <QSqlDatabase>
#include <QSqlError>
#include "log.h"

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
	out["interval"]	= _interval;

	return out;
}

void DatabaseConnectionInfo::fromJson(const Json::Value & json)
{
	//Log::log() << "DatabaseConnectionInfo::fromJson got:\n" << json << std::endl;
	
	_dbType		= DbTypeFromString(	json["dbType"]		.asString() )	;
	_username	= tq(				json["username"]	.asString() )	;
	_password	= tq(				json["password"]	.asString() )	;
	_database	= tq(				json["database"]	.asString() )	;
	_hostname	= tq(				json["hostname"]	.asString() )	;
	_query		= tq(				json["query"]		.asString() )	;
	_port		=					json["port"]		.asUInt()		;
	_interval	=					json["interval"]	.asInt()		;

}

bool DatabaseConnectionInfo::connect() const
{
	QSqlDatabase db = QSqlDatabase::addDatabase("QODBC");//DbTypeToQString(_dbType));

	db.setDatabaseName(	_database);
	db.setHostName(		_hostname);
	db.setUserName(		_username);
	db.setPassword(		_password);
	db.setPort(			_port);

	return db.open();
}

void DatabaseConnectionInfo::close() const
{
	QSqlDatabase::database().close();
}

QString DatabaseConnectionInfo::lastError() const
{
	return QSqlDatabase::database().lastError().text();	
}

QSqlQuery DatabaseConnectionInfo::runQuery() const
{
	if(!QSqlDatabase::database().isOpen())
		throw std::runtime_error(fq(QObject::tr("JASP thinks it's connected to the database but the QSqlDatabase isn't opened...")));
	
	QSqlQuery query;
	query.setForwardOnly(true);

	if(!query.exec(_query))
		throw std::runtime_error(fq(QObject::tr("Query failed with: '%1'").arg(query.lastError().text())));

	if(!query.isSelect())
		throw std::runtime_error(fq(QObject::tr("Query wasn't a SELECT-like statement and returned nothing.")));

	if(!query.isActive())
		throw std::runtime_error(fq(QObject::tr("No active result found, maybe there is something wrong with your query?")));
	
	return query;
}


