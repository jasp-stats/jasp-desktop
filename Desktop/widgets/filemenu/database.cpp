#include "database.h"
#include "utilities/settings.h"
#include <QtSql/QSqlDatabase>
#include <QtSql/QSqlQuery>
#include <QtSql/QSqlError>
#include <QtSql/QSqlRecord>
#include "log.h"
#include "utilities/qutils.h"

const QStringList Database::dbTypes()
{
	//Should be the *exact* same as DbType definition!
	return {	tr("Choose a database driver..."					),
				tr("IBM DB2"										),
				tr("Borland InterBase Driver"						),
				tr("MySQL Driver"									),
				tr("Oracle Call Interface Driver"					),
				tr("ODBC Driver (includes Microsoft SQL Server)"	),
				tr("PostgreSQL Driver"								),
				tr("SQLite version 3 or above"						),
				tr("SQLite version 2"								),
				tr("Sybase Adaptive Server"							) };
}

Database::Database(QObject *parent)
	: FileMenuObject{parent}
{
	_dbType			= static_cast<DbType>(	Settings::value( Settings::DB_IMPORT_TYPE		).toUInt());
	_database		=						Settings::value( Settings::DB_IMPORT_DBNAME		).toString();
	_hostname		=						Settings::value( Settings::DB_IMPORT_HOSTNAME	).toString();
	_port			=						Settings::value( Settings::DB_IMPORT_PORT		).toInt();
	_username		=						Settings::value( Settings::DB_IMPORT_USERNAME	).toString();
	_password		= decrypt(				Settings::value( Settings::DB_IMPORT_PASSWORD	).toString());
	_query			=						Settings::value( Settings::DB_IMPORT_QUERY		).toString();
}

void Database::connect()
{
	setConnected(false);

	QSqlDatabase db = QSqlDatabase::addDatabase(DbTypeToQString(DbType::QODBC));

	db.setDatabaseName(	_database);
	db.setHostName(		_hostname);
	db.setUserName(		_username);
	db.setPassword(		_password);
	db.setPort(			_port);

	setConnected(db.open());

	setLastError(connected() ? "Connected!" : "Error: " + db.lastError().text());
}

void Database::runQuery(const QString & query)
{
	setQueryResult(_runQuery(query));
}

void Database::setDbTypeFromIndex(int dbTypeIdx)
{
	setDbType(static_cast<DbType>(dbTypeIdx));
}

QString	Database::_runQuery(const QString & queryText)
{
	if(!connected())
		return tr("Not connected to database!");

	QSqlDatabase db = QSqlDatabase::database();

	if(!db.isOpen())
		return tr("JASP thinks it's connected to the database but the QSqlDatabase isn't opened...");
	
	QSqlQuery query;
	query.setForwardOnly(true);

	if(!query.exec(queryText))
		return tr("Query failed with: '%1'").arg(query.lastError().text());

	if(!query.isSelect())
		return tr("Query wasn't a SELECT-like statement and returned nothing.");

	if(!query.isActive())
		return tr("No active result found, maybe there is something wrong with your query?");

	QStringList fewLines;

	
	QSqlRecord  record = query.record();

	{
		QStringList names;

		for(int i=0; i<record.count(); i++)
			names.push_back(record.fieldName(i));

		fewLines.push_back(names.join(", "));
	}
	
	do
	{
		QStringList values;

		for(int i=0; i<record.count(); i++)
			values.push_back(query.value(i).toString());

		fewLines.push_back(values.join(", "));
	}
	while(query.next() && fewLines.size() < 10);
	

	return fewLines.join("\n");
}

void Database::setUsername(const QString &newUsername)
{
	if (_username == newUsername)
		return;
	
	_username = newUsername;
	Settings::setValue(Settings::DB_IMPORT_USERNAME, _username);
	
	emit usernameChanged();
}

void Database::setPassword(const QString &newPassword)
{
	if (_password == newPassword)
		return;
	
	_password = newPassword;
	Settings::setValue(Settings::DB_IMPORT_PASSWORD, encrypt(_password));
	
	emit passwordChanged();
}

void Database::setDatabase(const QString &newDatabase)
{
	if (_database == newDatabase)
		return;
	
	_database = newDatabase;
	Settings::setValue(Settings::DB_IMPORT_DBNAME, _database);
	
	emit databaseChanged();
}

void Database::setHostname(const QString &newHostname)
{
	if (_hostname == newHostname)
		return;
	
	_hostname = newHostname;
	Settings::setValue(Settings::DB_IMPORT_HOSTNAME, _hostname);
	
	emit hostnameChanged();
}

void Database::setDbType(const DbType newDbType)
{
	if (_dbType == newDbType)
		return;
	
	_dbType = newDbType;
	Settings::setValue(Settings::DB_IMPORT_TYPE, static_cast<uint>(_dbType));
	
	emit dbTypeChanged();
}

void Database::setConnected(bool newConnected)
{
	if (_connected == newConnected)
		return;
	_connected = newConnected;
	emit connectedChanged();
}

void Database::setQueryResult(const QString &newQueryResult)
{
	if (_queryResult == newQueryResult)
		return;
	
	_queryResult = newQueryResult;
	
	emit queryResultChanged();
}

const QString &Database::lastError() const
{
	return _lastError;
}

void Database::setLastError(const QString &newLastError)
{
	if (_lastError == newLastError)
		return;
	_lastError = newLastError;
	emit lastErrorChanged();
}

int Database::port() const
{
	return _port;
}

void Database::setPort(int newPort)
{
	if (_port == newPort)
		return;
	
	_port = newPort;
	Settings::setValue(Settings::DB_IMPORT_PORT, _port);
	
	emit portChanged();
}

void Database::setQuery(const QString &newQuery)
{
	if (_query == newQuery)
		return;
	
	_query = newQuery;
	Settings::setValue(Settings::DB_IMPORT_QUERY, _query);
	
	emit queryChanged();
}
