//
// Copyright (C) 2013-2026 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//
#ifndef FILEEVENT_H
#define FILEEVENT_H

#include <QObject>
#include <QString>
#include <QPointer>
#include "json/json.h"
#include "utilenums.h"
#include "fileeventstatus.h"

class Exporter;
class DataSet;

///
/// This class is used to handle the communication to and from the asynchronous loading/synching/saving file processes.
/// These can be chained to have a Close come after a save once the latter is done.
class FileEvent : public QObject
{
	Q_OBJECT

public:
	enum FileMode { FileSave, FileNew, FileOpen, FileExportResults, FileExportData, FileGenerateData, FileSyncData, FileClose };

	FileEvent(QObject *parent, FileMode fileMode = FileEvent::FileOpen, bool routeThroughFileMenu = true); ///< routeThroughFileMenu=false for UI-independent events (the data-syncer path) that drive the loader themselves.
	virtual	~FileEvent();

	bool				setPath(		const QString & path);
	QString path() const { return !_tmp ? _path : pathTmp(); }
	static QString		pathTmp();
	void				setDataFilePath(const QString & path);
	const QString &		dataFilePath() const { return _dataFilePath;	}
	void				setOsfPath(const QString & path)		{ _osfPath = path; }
	const QString &		osfPath() const { return _osfPath;		}
	void				setDatabase(	const Json::Value & dbInfo);
	void				setDataSet(DataSet * ds);		///< defined in fileevent.cpp (needs a complete DataSet)
	DataSet		*		dataSet()							const;
	int					dataSetId()							const { return _dataSetId; }
	void				setFileType(	Utils::FileType	type)			{ _type = type; }
	void				setTmp(			bool saveTmp)					{ _tmp  = saveTmp; }

	void				starts();
	void				setComplete(bool success = true, const QString &message = "", bool cancelled = false);
	void				cleanUp();
	void				chain(FileEvent *event, bool resetDataSet = false);

	bool				isDatabase()	const { return _database != Json::nullValue;	}
	bool				isOnlineNode()	const { return _path.startsWith("http");		}
	bool				isExample()		const;
	bool				isReadOnly()	const { return isExample() || isDatabase();		}
	bool				isStarted()		const { return _status != FileEventStatus::Initialized;	}
	bool				isCompleted()	const { return _status == FileEventStatus::Completed;		}
	bool				isSuccessful()	const { return isCompleted() && _success;					}
	bool				isCancelled()	const { return _cancelled;									}
	bool				isTmp()			const { return _tmp; }
	static bool			autoSaveExists();
	static void			removeAutoSaveIfItExists();

	Exporter *			exporter()		const { return _exporter;		}
	FileMode			operation()		const { return _operation;		}
	Utils::FileType		type()			const { return _type;			}

	const std::string	databaseStr()	const;
	const Json::Value &	database()		const { return _database;		}
	const QString &		message()		const { return _message;		}
	const QString &		getLastError()	const { return _last_error;		}
	QString				getProgressMsg() const;
	void				setSilent(bool newSilent);

signals:
	void started();
	void completed();
	void finalized();

private:
	FileMode			_operation;
	QPointer<DataSet>	_dataSet;			///< Auto-nulls if the target dataset is destroyed before the event runs
	int					_dataSetId	= -1;	///< id snapshot taken at bind time, survives destruction of the DataSet
	Utils::FileType		_type			= Utils::FileType::unknown;
	QString				_path,
						_osfPath		= "", //To show the user a friendly path
						_dataFilePath,
						_last_error		= "Unknown error",
						_message;
	FileEventStatus		_status			= FileEventStatus::Initialized;
	bool				_success		= false,
						_cancelled		= false,
						_tmp			= false;
	Exporter		*	_exporter		= nullptr;
	Json::Value			_database		= Json::nullValue;
};

#endif // FILEEVENT_H


