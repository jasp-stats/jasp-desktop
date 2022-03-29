#ifndef DATABASE_H
#define DATABASE_H

#include "filemenuobject.h"
#include "utilenums.h"

class Database : public FileMenuObject
{
	Q_OBJECT

	Q_PROPERTY(DbType		dbType		READ dbType			WRITE setDbType			NOTIFY dbTypeChanged		)
	Q_PROPERTY(QString		database	READ database		WRITE setDatabase		NOTIFY databaseChanged		)
	Q_PROPERTY(QString		hostname	READ hostname		WRITE setHostname		NOTIFY hostnameChanged		)
	Q_PROPERTY(QString		username	READ username		WRITE setUsername		NOTIFY usernameChanged		)
	Q_PROPERTY(QString		password	READ password		WRITE setPassword		NOTIFY passwordChanged		)
	Q_PROPERTY(bool			connected	READ connected		WRITE setConnected		NOTIFY connectedChanged		)
	Q_PROPERTY(QString		queryResult READ queryResult	WRITE setQueryResult	NOTIFY queryResultChanged	)
	Q_PROPERTY(QString		query		READ query			WRITE setQuery			NOTIFY queryChanged			)
	Q_PROPERTY(QStringList	dbTypes		READ dbTypes								NOTIFY dbTypesChanged		)
	Q_PROPERTY(QString		lastError	READ lastError								NOTIFY lastErrorChanged		)
	Q_PROPERTY(int			port		READ port			WRITE setPort			NOTIFY portChanged			)

public:
	explicit Database(QObject *parent = nullptr);

	Q_INVOKABLE void	connect();
	Q_INVOKABLE void	runQuery(const QString & query);
	Q_INVOKABLE void	setDbTypeFromIndex(int dbTypeIdx);

	const DbType			dbType()		const { return _dbType;			}
	const QString		&	hostname()		const { return _hostname;		}
	const QString		&	database()		const { return _database;		}
	const QString		&	username()		const { return _username;		}
	const QString		&	password()		const { return _password;		}
	bool					connected()		const { return _connected;		}
	const QString		&	queryResult()	const { return _queryResult;	}
	const QString		&	lastError()		const;
	int						port()			const;
	const QString		&	query()			const { return _query;			}

	static const QStringList		dbTypes();

	void setDbType(		const DbType	newDbType		);
	void setHostname(	const QString & newHostname		);
	void setDatabase(	const QString & newDatabase		);
	void setUsername(	const QString & newUsername		);
	void setPassword(	const QString & newPassword		);
	void setQuery(		const QString &	newQuery		);
	void setPort(		int				newPort			);
	void setConnected(	bool			newConnected	);
	void setQueryResult(const QString & newQueryResult	);
	void setLastError(	const QString &	newLastError	);
	
signals:
	void dbTypeChanged();
	void usernameChanged();
	void passwordChanged();
	void databaseChanged();
	void hostnameChanged();
	void connectedChanged();
	void queryResultChanged();
	void dbTypesChanged();
	void lastErrorChanged();
	void portChanged();
	
	void queryChanged();
	
private:
	QString	_runQuery(const QString & query);

private:
	DbType  _dbType			= DbType::NOTCHOSEN;
	QString _username		= "",
			_password		= "",
			_database		= "",
			_hostname		= "",
			_queryResult	= "",
			_lastError		= "",
			_query			= "";
	bool	_connected		= false;
	int		_port			= 0;
};

#endif // DATABASE_H
