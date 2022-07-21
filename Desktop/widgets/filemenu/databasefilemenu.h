#ifndef DATABASEFILEMENU_H
#define DATABASEFILEMENU_H

#include "filemenuobject.h"
#include "utilenums.h"
#include "data/databaseconnectioninfo.h"

class DatabaseFileMenu : public FileMenuObject
{
	typedef DatabaseConnectionInfo Info;
	
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
	Q_PROPERTY(bool			resultsOK	READ resultsOK		WRITE setResultsOK		NOTIFY resultsOKChanged		)
	Q_PROPERTY(int			interval	READ interval		WRITE setInterval		NOTIFY intervalChanged		)
	Q_PROPERTY(bool			dbMaybeFile	READ dbMaybeFile							NOTIFY dbTypeChanged		)
	Q_PROPERTY(bool			rememberMe	READ rememberMe		WRITE setRememberMe		NOTIFY rememberMeChanged	)

public:
	explicit					DatabaseFileMenu(QObject *parent = nullptr);

	Q_INVOKABLE void			connect();
	Q_INVOKABLE void			runQuery();
	Q_INVOKABLE void			importResults();
	Q_INVOKABLE void			setDbTypeFromIndex(int dbTypeIdx);
	Q_INVOKABLE void			browseDbFile();

	const DbType				dbType()			const { return _info._dbType;						}
	const QString		&		hostname()			const { return _info._hostname;						}
	const QString		&		database()			const { return _info._database;						}
	const QString		&		username()			const { return _info._username;						}
	const QString		&		password()			const { return _info._password;						}
	bool						connected()			const { return _connected;							}
	const QString		&		queryResult()		const { return _queryResult;						}
	const QString		&		lastError()			const { return _lastError;							}
	int							port()				const { return _info._port;							}
	const QString		&		query()				const { return _info._query;						}
	bool						resultsOK()			const { return _resultsOK;							}
	int							interval()			const { return _info._interval;						}
	bool						dbMaybeFile()		const { return _info._dbType == DbType::QSQLITE;	}
	const bool					rememberMe()		const { return _info._rememberMe;					}

	bool						readyForImport()	const;
	void						resetEphemeralFields();

	static const QStringList	dbTypes();

	void						setDbType(		const DbType	newDbType		);
	void						setHostname(	const QString & newHostname		);
	void						setDatabase(	const QString & newDatabase		);
	void						setUsername(	const QString & newUsername		);
	void						setPassword(	const QString & newPassword		);
	void						setQuery(		const QString &	newQuery		);
	void						setPort(		int				newPort			);
	void						setConnected(	bool			newConnected	);
	void						setQueryResult(const QString & newQueryResult	);
	void						setLastError(	const QString &	newLastError	);
	void						setResultsOK(	bool			newResultsOK	);
	void						setInterval(	int				newInterval		);
	void						setRememberMe(	bool			rememberMe		);

signals:
	void						dbTypeChanged();
	void						usernameChanged();
	void						passwordChanged();
	void						databaseChanged();
	void						hostnameChanged();
	void						connectedChanged();
	void						queryResultChanged();
	void						dbTypesChanged();
	void						lastErrorChanged();
	void						portChanged();
	void						queryChanged();
	void						resultsOKChanged();
	void						intervalChanged();
	void						rememberMeChanged();
	
private:
	QString						_runQuery();

private:
	Info						_info;
	QString						_queryResult	= "",
								_lastError		= "";
	bool						_connected		= false,
								_resultsOK		= false;
};

#endif // DATABASEFILEMENU_H
