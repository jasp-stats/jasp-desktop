#include "log.h"
#include "qutils.h"
#include <QSqlError>
#include <QSqlDatabase>
#include <QCoreApplication>
#include "databaseconnectioninfo.h"

Json::Value DatabaseConnectionInfo::toJson(bool forJaspFile) const
{
	Json::Value out = Json::objectValue;
	
	//Log::log() << "DatabaseConnectionInfo::toJson has dbType :\n" << _dbType << std::endl;
	//Log::log() << "DatabaseConnectionInfo::toJson has DbTypeToString(_dbType) :\n" << DbTypeToString(_dbType) << std::endl;
	
	out["dbType"]		= DbTypeToString(_dbType);
	out["username"]		= fq(_username);
	out["password"]		= (!forJaspFile || _rememberMe) ? fq(_password) : "";
	out["database"]		= fq(_database);
	out["hostname"]		= fq(_hostname);
	out["query"]		= fq(_query);
	out["port"]			= _port;
	out["interval"]		= _interval;
	out["rememberMe"]	= _rememberMe;
	out["hadPassword"]	= forJaspFile ? _password != "" : _hadPassword;

	return out;
}

void DatabaseConnectionInfo::init()
{
	QObject::connect(&_syncher,	&QTimer::timeout, this,	&DatabaseConnectionInfo::synchingIntervalPassed);
}

void DatabaseConnectionInfo::fromJson(const Json::Value & json)
{
	//Log::log() << "DatabaseConnectionInfo::fromJson got:\n" << json << std::endl;
	
	_dbType			= DbTypeFromString(	json["dbType"]		.asString() )	;
	_username		= tq(				json["username"]	.asString() )	;
	_password		= tq(				json["password"]	.asString() )	;
	_database		= tq(				json["database"]	.asString() )	;
	_hostname		= tq(				json["hostname"]	.asString() )	;
	_query			= tq(				json["query"]		.asString() )	;
	_port			=					json["port"]		.asUInt()		;
	_interval		=					json["interval"]	.asInt()		;
	_rememberMe		=					json["rememberMe"]	.asBool()		;
	_hadPassword	=					json["hadPassword"]	.asBool()		;

}

bool DatabaseConnectionInfo::connect() const
{
	QString			dbTypeString	= DbTypeToQString(_dbType);
	//Log::log() << "dbTypeString is '" << dbTypeString << "'" << std::endl;

	if(_connectionName.isEmpty())
		_connectionName = QString("JASP_%1_%2").arg(dbTypeString).arg(reinterpret_cast<quintptr>(this));

	QSqlDatabase	db				= QSqlDatabase::addDatabase(dbTypeString, _connectionName);

	if(_database.size())	db.setDatabaseName(	_database);
	if(_hostname.size())	db.setHostName(		_hostname);
	if(_username.size())	db.setUserName(		_username);
	if(_password.size())	db.setPassword(		_password);
	if(_port)				db.setPort(			_port);

	return db.open();
}

bool DatabaseConnectionInfo::connected() const
{
	return _connectionName.size() && QSqlDatabase::database(_connectionName).isOpen();
}

void DatabaseConnectionInfo::close() const
{
	if(_connectionName.size())
		QSqlDatabase::database(_connectionName).close();
}

QString DatabaseConnectionInfo::lastError() const
{
	return _connectionName.size() ? QSqlDatabase::database(_connectionName).lastError().text() : QString();
}

QSqlQuery DatabaseConnectionInfo::runQuery() const
{
	if(!connected())
		throw std::runtime_error(fq(tr("JASP thinks it's connected to the database but the QSqlDatabase isn't opened...")));
	
	QSqlQuery query(QSqlDatabase::database(_connectionName));
	query.setForwardOnly(true);

	if(!query.exec(_query))
		throw std::runtime_error(fq(tr("Query failed with: '%1'").arg(query.lastError().text())));

	if(!query.isSelect())
		throw std::runtime_error(fq(tr("Query wasn't a SELECT-like statement and returned nothing.")));

	if(!query.isActive())
		throw std::runtime_error(fq(tr("No active result found, maybe there is something wrong with your query?")));
	
	return query;
}

bool DatabaseConnectionInfo::startSynching(bool syncImmediately)
{
	if(_interval > 0)
	{
		if(_hadPassword && !_rememberMe && _password == "")
		{
			bool tryAgain = true, couldConnect;

			while(tryAgain)
			{
				_password	= emit askPassword(tr("Database Password"), _username != "" ? tr("The databaseconnection needs a password for user '%1'").arg(_username) : tr("The databaseconnection needs a password"));
				tryAgain		= connect() ? false :  emit showYesNo(tr("Connection failed"), tr("Could not connect to database because of '%1', want to try a different password?").arg(lastError()));
			}

			if(!connected())
			{
				emit showWarning(tr("Database connection failed"), tr("Could not connect to the database so synchronizing will be disabled."));
				return false;
			}
		}

		_syncher.setInterval(1000 * 60 * _interval);
		_syncher.start();

		if(syncImmediately)
			emit synchingIntervalPassed();

		emit synchingChanged();
		
		return true;
	}

	return false;
}

void DatabaseConnectionInfo::stopSynching()
{
	if(!_syncher.isActive())
		return;
	
	_syncher.stop();
	emit synchingChanged();
}


