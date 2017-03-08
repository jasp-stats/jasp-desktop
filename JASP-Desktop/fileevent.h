//
// Copyright (C) 2013-2017 University of Amsterdam
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

class FileEvent : public QObject
{
	Q_OBJECT

public:
	enum FileMode { FileSave, FileOpen, FileExportResults, FileExportData, FileSyncData, FileClose };

    FileEvent(QObject *parent = NULL, FileMode fileMode = FileEvent::FileOpen);
	virtual ~FileEvent();
	FileEvent(const FileEvent&) = default;

	bool setPath(const QString &path);
	void setDataFilePath(const QString &path);
	void setReadOnly();
	Exporter *getExporter() const {return _exporter;}
	QString getLastError() const;

	bool IsOnlineNode() const;

	FileMode operation() const;
	Utils::FileType type() const;
	const QString &path() const;
	const QString &dataFilePath() const;
	bool isReadOnly() const;

	void setComplete(bool success = true, const QString &message = "");

	bool isCompleted() const;
	bool successful() const;
	const QString &message() const;

	void chain(FileEvent *event);

signals:
	void completed(FileEvent *event);
	void dataFileChanged(QString dataFilePath);

private slots:
	void chainedComplete(FileEvent *event);

private:
	FileMode _operation;
	Utils::FileType _type;
	QString _path;
	QString _dataFilePath;
	QString _last_error;
	bool _readOnly;

	bool _isComplete;
	bool _success;
	QString _message;

	FileEvent *_chainedTo;

	Exporter *_exporter;
};

Q_DECLARE_METATYPE(FileEvent *)
#endif // FILEEVENT_H
