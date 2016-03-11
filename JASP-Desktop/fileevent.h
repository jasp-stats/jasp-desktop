//
// Copyright (C) 2013-2016 University of Amsterdam
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

class FileEvent : public QObject
{
	Q_OBJECT

public:
	FileEvent(QObject *parent = NULL);
	~FileEvent() = default;
	FileEvent(const FileEvent&) = default;

	enum FileMode { FileSave, FileOpen, FileExport, FileClose };

	void setOperation(FileMode fileMode);
	void setPath(const QString &path);
	void setReadOnly();

	bool IsOnlineNode() const;

	FileMode operation() const;
	const QString &path() const;
	bool isReadOnly() const;

	void setComplete(bool success = true, const QString &message = "");

	bool isCompleted() const;
	bool successful() const;
	const QString &message() const;

	void chain(FileEvent *event);

signals:
	void completed(FileEvent *event);

private slots:
	void emitComplete();
	void chainedComplete(FileEvent *event);

private:
	FileMode _operation;
	QString _path;
	bool _readOnly;

	bool _isComplete;
	bool _success;
	QString _message;

	FileEvent *_chainedTo;

};

Q_DECLARE_METATYPE(FileEvent *)
#endif // FILEEVENT_H
