//
// Copyright (C) 2013-2018 University of Amsterdam
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
#include <QMetaType>

#include "exporters/exporter.h"
#include "utilenums.h"

///
/// This class is used to handle the communication to and from the asynchronous loading/synching/saving file processes.
/// These can be chained to have a Close come after a save once the latter is done.
class FileEvent : public QObject
{
	Q_OBJECT

public:
	enum FileMode { FileSave, FileOpen, FileExportResults, FileExportData, FileGenerateData, FileSyncData, FileClose };

					FileEvent(QObject *parent = nullptr, FileMode fileMode = FileEvent::FileOpen);
	virtual			~FileEvent();

	bool				setPath(		const QString & path);
	void				setDataFilePath(const QString & path);
	void				setOsfPath(		const QString & path) { _osfPath = path; }
	void				setDatabase(	const Json::Value & dbInfo);

	void				setComplete(bool success = true, const QString &message = "");
	void				chain(FileEvent *event);

	void				setReadOnly()		  { _readOnly = true;		}

	bool				isDatabase()	const { return _database != Json::nullValue;	}
	bool				isOnlineNode()	const { return _path.startsWith("http");		}
	bool				isReadOnly()	const { return _readOnly;						}
	bool				isCompleted()	const { return _completed;						}
	bool				isSuccessful()	const { return _success;						}

	Exporter *			exporter()		const { return _exporter;		}
	FileMode			operation()		const { return _operation;		}
	Utils::FileType		type()			const { return _type;			}

	const QString &		path()			const { return _path;			}
	const std::string	database()		const;
	const QString &		osfPath()		const { return _osfPath;		}
	const QString &		dataFilePath()	const { return _dataFilePath;	}
	const QString &		message()		const { return _message;		}
	const QString &		getLastError()	const { return _last_error;		}

	QString				getProgressMsg() const;

signals:
	void completed(FileEvent *event);

private slots:
	void chainedComplete(FileEvent *event) { setComplete(event->isSuccessful(), event->message()); }

private:
	FileMode			_operation;
	Utils::FileType		_type;
	QString				_path,
						_osfPath		= "", //To show the user a friendly path
						_dataFilePath,
						_last_error		= "Unknown error",
						_message;
	bool				_readOnly		= false,
						_completed		= false,
						_success		= false;
	FileEvent		*	_chainedTo		= nullptr;
	Exporter		*	_exporter		= nullptr;
	Json::Value			_database		= Json::nullValue;
};

Q_DECLARE_METATYPE(FileEvent *)
Q_DECLARE_METATYPE(FileEvent::FileMode)
#endif // FILEEVENT_H
