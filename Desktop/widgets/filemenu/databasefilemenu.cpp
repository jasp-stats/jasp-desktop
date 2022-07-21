#include "databasefilemenu.h"
#include "utilities/settings.h"
#include <QtSql/QSqlQuery>
#include <QtSql/QSqlError>
#include <QtSql/QSqlRecord>
#include "log.h"
#include "utilities/qutils.h"
#include "gui/messageforwarder.h"

const QStringList DatabaseFileMenu::dbTypes()
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

DatabaseFileMenu::DatabaseFileMenu(FileMenu *parent)
	: FileMenuObject{parent}
{
	_info._dbType		= static_cast<DbType>(	Settings::value( Settings::DB_IMPORT_TYPE		).toUInt());
	_info._database		=						Settings::value( Settings::DB_IMPORT_DBNAME		).toString();
	_info._hostname		=						Settings::value( Settings::DB_IMPORT_HOSTNAME	).toString();
	_info._port			=						Settings::value( Settings::DB_IMPORT_PORT		).toInt();
	_info._username		=						Settings::value( Settings::DB_IMPORT_USERNAME	).toString();
	_info._password		= decrypt(				Settings::value( Settings::DB_IMPORT_PASSWORD	).toString());
	_info._query		=						Settings::value( Settings::DB_IMPORT_QUERY		).toString();
	_info._interval		=						Settings::value( Settings::DB_IMPORT_INTERVAL	).toInt();
	_info._rememberMe	=						Settings::value( Settings::DB_REMEMBER_ME		).toBool();
}

void DatabaseFileMenu::connect()
{
	setConnected(_info.connect());
	setLastError(connected() ? "Connected!" : "Error: " + _info.lastError());
}

bool DatabaseFileMenu::readyForImport() const
{
	return connected() && resultsOK();
}

void DatabaseFileMenu::importResults()
{
	if(!readyForImport())
	{
		Log::log() << "DatabaseFileMenu::importResults() called without it being ready for import... Should not be possible but ok..." << std::endl;
		return;
	}
	
	_info.close();
	
	FileEvent *event = new FileEvent(this, mode());
	
	event->setFileType(FileTypeBase::database);

	event->setDatabase(_info.toJson());

	emit dataSetIORequest(event);
	
	resetEphemeralFields();
}

void DatabaseFileMenu::resetEphemeralFields()
{
	setConnected(	false);
	setQueryResult(	"");
	setLastError(	"");
	setResultsOK(	false);
}

void DatabaseFileMenu::setDbTypeFromIndex(int dbTypeIdx)
{
	if(dbTypeIdx > 0 && dbTypeIdx <= int(DbType::QSQLITE)) //is it really a DbType?
		setDbType(static_cast<DbType>(dbTypeIdx));
}

void DatabaseFileMenu::runQuery()
{
	setQueryResult(_runQuery());
}

QString	DatabaseFileMenu::_runQuery()
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


void DatabaseFileMenu::browseDbFile()
{
	QString file = MessageForwarder::msgForwarder()->browseOpenFileDocuments("Select your database file", "*");

	if(file.size())
		setDatabase(file);

}

void DatabaseFileMenu::setUsername(const QString &newUsername)
{
	if (_info._username == newUsername)
		return;
	
	_info._username = newUsername;
	Settings::setValue(Settings::DB_IMPORT_USERNAME, _info._username);
	
	emit usernameChanged();
}

void DatabaseFileMenu::setPassword(const QString &newPassword)
{
	if (_info._password == newPassword)
		return;
	
	_info._password = newPassword;
	
	if(_info._rememberMe)
		Settings::setValue(Settings::DB_IMPORT_PASSWORD, encrypt(_info._password));
	
	emit passwordChanged();
}

void DatabaseFileMenu::setRememberMe(bool rememberMe)
{
	if (_info._rememberMe == rememberMe)
		return;
	
	_info._rememberMe = rememberMe;
	Settings::setValue(Settings::DB_REMEMBER_ME, _info._rememberMe);
	
	if(_info._rememberMe)	Settings::setValue(Settings::DB_IMPORT_PASSWORD, encrypt(_info._password));
	else					Settings::setValue(Settings::DB_IMPORT_PASSWORD, "");
	
	emit rememberMeChanged();
}

void DatabaseFileMenu::setDatabase(const QString &newDatabase)
{
	if (_info._database == newDatabase)
		return;
	
	_info._database = newDatabase;
	Settings::setValue(Settings::DB_IMPORT_DBNAME, _info._database);
	
	emit databaseChanged();
}

void DatabaseFileMenu::setHostname(const QString &newHostname)
{
	if (_info._hostname == newHostname)
		return;
	
	_info._hostname = newHostname;
	Settings::setValue(Settings::DB_IMPORT_HOSTNAME, _info._hostname);
	
	emit hostnameChanged();
}

void DatabaseFileMenu::setDbType(const DbType newDbType)
{
	if (_info._dbType == newDbType)
		return;

	_info._dbType = newDbType;
	Settings::setValue(Settings::DB_IMPORT_TYPE, static_cast<uint>(_info._dbType));
	
	emit dbTypeChanged();
}

void DatabaseFileMenu::setConnected(bool newConnected)
{
	if (_connected == newConnected)
		return;
	_connected = newConnected;
	emit connectedChanged();
}

void DatabaseFileMenu::setQueryResult(const QString &newQueryResult)
{
	if (_queryResult == newQueryResult)
		return;
	
	_queryResult = newQueryResult;
	
	emit queryResultChanged();
}

void DatabaseFileMenu::setLastError(const QString &newLastError)
{
	if (_lastError == newLastError)
		return;
	
	_lastError = newLastError;
	
	emit lastErrorChanged();
}

void DatabaseFileMenu::setPort(int newPort)
{
	if (_info._port == newPort)
		return;
	
	_info._port = newPort;
	Settings::setValue(Settings::DB_IMPORT_PORT, _info._port);
	
	emit portChanged();
}

void DatabaseFileMenu::setQuery(const QString &newQuery)
{
	if (_info._query == newQuery)
		return;
	
	_info._query = newQuery;
	Settings::setValue(Settings::DB_IMPORT_QUERY, _info._query);
	
	emit queryChanged();
}

void DatabaseFileMenu::setResultsOK(bool newResultsOK)
{
	if (_resultsOK == newResultsOK)
		return;
	_resultsOK = newResultsOK;
	emit resultsOKChanged();
}

void DatabaseFileMenu::setInterval(int newInterval)
{
	if (_info._interval == newInterval)
		return;
	
	_info._interval = newInterval;
	Settings::setValue(Settings::DB_IMPORT_INTERVAL, _info._interval);
	
	emit intervalChanged();
}


