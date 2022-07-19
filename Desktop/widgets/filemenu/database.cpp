#include "database.h"
#include "utilities/settings.h"
#include <QtSql/QSqlQuery>
#include <QtSql/QSqlError>
#include <QtSql/QSqlRecord>
#include "log.h"
#include "utilities/qutils.h"
#include "utilities/messageforwarder.h"

const QStringList Database::dbTypes()
{
	//Should be the *exact* same as DbType definition!
	return {	tr("Choose a database driver..."					),
				tr("IBM DB2"										),
				
				/*tr("Borland InterBase Driver"						),*/

				tr("MySQL Driver"									),
				tr("Oracle Call Interface Driver"					),
				tr("ODBC Driver (includes Microsoft SQL Server)"	),
				tr("PostgreSQL Driver"								),
				tr("SQLite version 3 or above"						)
				
				/*,
				tr("SQLite version 2"								),
				tr("Sybase Adaptive Server"							)*/ };
}

Database::Database(QObject *parent)
	: FileMenuObject{parent}
{
	_info._dbType	= static_cast<DbType>(	Settings::value( Settings::DB_IMPORT_TYPE		).toUInt());
	_info._database	=						Settings::value( Settings::DB_IMPORT_DBNAME		).toString();
	_info._hostname	=						Settings::value( Settings::DB_IMPORT_HOSTNAME	).toString();
	_info._port		=						Settings::value( Settings::DB_IMPORT_PORT		).toInt();
	_info._username	=						Settings::value( Settings::DB_IMPORT_USERNAME	).toString();
	_info._password	= decrypt(				Settings::value( Settings::DB_IMPORT_PASSWORD	).toString());
	_info._query	=						Settings::value( Settings::DB_IMPORT_QUERY		).toString();
	_info._interval	=						Settings::value( Settings::DB_IMPORT_INTERVAL	).toInt();
}

void Database::connect()
{
	setConnected(_info.connect());
	setLastError(connected() ? "Connected!" : "Error: " + _info.lastError());
}

bool Database::readyForImport() const
{
	return connected() && resultsOK();
}

void Database::importResults()
{
	if(!readyForImport())
	{
		Log::log() << "Database::importResults() called without it being ready for import... Should not be possible but ok..." << std::endl;
		return;
	}
	
	_info.close();
	
	FileEvent *event = new FileEvent(this, _mode);

	event->setDatabase(_info.toJson());
	emit dataSetIORequest(event);
}

void Database::setDbTypeFromIndex(int dbTypeIdx)
{
	if(dbTypeIdx > 0 && dbTypeIdx <= int(DbType::QSQLITE)) //is it really a DbType?
		setDbType(static_cast<DbType>(dbTypeIdx));
}

void Database::runQuery()
{
	setQueryResult(_runQuery());
}

QString	Database::_runQuery()
{
	setResultsOK(false);
	
	if(!connected())
		return tr("Not connected to database!");
	
	try
	{
		QSqlQuery query = _info.runQuery();
	
		QStringList fewLines;
	
		QSqlRecord  record = query.record();
	
		{
			QStringList names;
	
			for(int i=0; i<record.count(); i++)
				names.push_back(record.fieldName(i));
	
			fewLines.push_back(names.join(", "));
		}
		
		if(_info._dbType == DbType::QSQLITE)
			query.next(); //skip first empy line

		do
		{
			QStringList values;
	
			for(int i=0; i<record.count(); i++)
				values.push_back(query.value(i).toString());
	
			fewLines.push_back(values.join(", "));
		}
		while(query.next() && fewLines.size() < 4);
	
		setResultsOK(true); //I suppose the results must be ok if we get all the way here
	
		return fewLines.join("\n");
	}
	catch(std::runtime_error & e)
	{
		return tq(e.what());	
	}
}


void Database::browseDbFile()
{
	QString file = MessageForwarder::msgForwarder()->browseOpenFileDocuments("Select your database file", "*");

	if(file.size())
		setDatabase(file);

}

void Database::setUsername(const QString &newUsername)
{
	if (_info._username == newUsername)
		return;
	
	_info._username = newUsername;
	Settings::setValue(Settings::DB_IMPORT_USERNAME, _info._username);
	
	emit usernameChanged();
}

void Database::setPassword(const QString &newPassword)
{
	if (_info._password == newPassword)
		return;
	
	_info._password = newPassword;
	Settings::setValue(Settings::DB_IMPORT_PASSWORD, encrypt(_info._password));
	
	emit passwordChanged();
}

void Database::setDatabase(const QString &newDatabase)
{
	if (_info._database == newDatabase)
		return;
	
	_info._database = newDatabase;
	Settings::setValue(Settings::DB_IMPORT_DBNAME, _info._database);
	
	emit databaseChanged();
}

void Database::setHostname(const QString &newHostname)
{
	if (_info._hostname == newHostname)
		return;
	
	_info._hostname = newHostname;
	Settings::setValue(Settings::DB_IMPORT_HOSTNAME, _info._hostname);
	
	emit hostnameChanged();
}

void Database::setDbType(const DbType newDbType)
{
	if (_info._dbType == newDbType)
		return;

	_info._dbType = newDbType;
	Settings::setValue(Settings::DB_IMPORT_TYPE, static_cast<uint>(_info._dbType));
	
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

void Database::setLastError(const QString &newLastError)
{
	if (_lastError == newLastError)
		return;
	
	_lastError = newLastError;
	
	emit lastErrorChanged();
}

void Database::setPort(int newPort)
{
	if (_info._port == newPort)
		return;
	
	_info._port = newPort;
	Settings::setValue(Settings::DB_IMPORT_PORT, _info._port);
	
	emit portChanged();
}

void Database::setQuery(const QString &newQuery)
{
	if (_info._query == newQuery)
		return;
	
	_info._query = newQuery;
	Settings::setValue(Settings::DB_IMPORT_QUERY, _info._query);
	
	emit queryChanged();
}

void Database::setResultsOK(bool newResultsOK)
{
	if (_resultsOK == newResultsOK)
		return;
	_resultsOK = newResultsOK;
	emit resultsOKChanged();
}

void Database::setInterval(int newInterval)
{
	if (_info._interval == newInterval)
		return;
	
	_info._interval = newInterval;
	Settings::setValue(Settings::DB_IMPORT_INTERVAL, _info._interval);
	
	emit intervalChanged();
}

