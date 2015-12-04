//
// Copyright (C) 2013-2015 University of Amsterdam
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

#include "fileevent.h"

#include <QTimer>

FileEvent::FileEvent(QObject *parent)
	: QObject(parent)
{
	_operation = FileEvent::FileOpen;
	_readOnly = false;
	_chainedTo = NULL;
}

void FileEvent::setOperation(FileEvent::FileMode fileMode)
{
	_operation = fileMode;
}

void FileEvent::setPath(const QString &path)
{
	_path = path;
}

void FileEvent::setReadOnly()
{
	_readOnly = true;
}

FileEvent::FileMode FileEvent::operation() const
{
	return _operation;
}

const QString &FileEvent::path() const
{
	return _path;
}

bool FileEvent::isReadOnly() const
{
	return _readOnly;
}

void FileEvent::setComplete(bool success, const QString &message)
{
	_isComplete = true;
	_success = success;
	_message = message;

	emit completed(this);
}

bool FileEvent::isCompleted() const
{
	return _isComplete;
}

bool FileEvent::successful() const
{
	return _success;
}

const QString &FileEvent::message() const
{
	return _message;
}

void FileEvent::chain(FileEvent *event)
{
	_chainedTo = event;
	connect(event, SIGNAL(completed(FileEvent*)), this, SLOT(chainedComplete(FileEvent*)));
}

void FileEvent::emitComplete()
{
	emit completed(this);
}

void FileEvent::chainedComplete(FileEvent *event)
{
	setComplete(event->successful(), event->message());
}

bool FileEvent::IsOnlineNode() const
{
	return _path.startsWith("http");
}
