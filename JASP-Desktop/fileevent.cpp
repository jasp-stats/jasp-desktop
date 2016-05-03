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

#include "fileevent.h"
#include "exporters/dataexporter.h"
#include "exporters/resultexporter.h"
#include "exporters/jaspexporter.h"

#include <QTimer>

FileEvent::FileEvent(QObject *parent, FileEvent::FileMode fileMode)
	: QObject(parent)
{
	_readOnly = false;
	_chainedTo = NULL;
	_operation = fileMode;
	switch (fileMode)
	{
		case FileEvent::FileExportResults:
			{
				_exporter = new ResultExporter();
			}
			break;
		case FileEvent::FileExportData:
			{
				_exporter = new DataExporter();
			}
			break;
		case FileEvent::FileSave:
			{
				_exporter = new JASPExporter();
			}
			break;
		default:
			{
				_exporter = NULL;
			}
	}
}

FileEvent::~FileEvent()
{
	if (_exporter != NULL) {
		delete _exporter;
	}
}

void FileEvent::setType(FileEvent::FileType fileType)
{
	_type = fileType;
}

void FileEvent::setTypeFromPath(const QString &path)
{

	FileEvent::FileType filetype;

	filetype = getTypeFromPath(path);

	_type = filetype;
}

FileEvent::FileType FileEvent::getTypeFromPath(const QString &path)
{

	FileEvent::FileType filetype =  FileEvent::FileType::unknown;

	if (path.endsWith(".jasp", Qt::CaseInsensitive)) filetype = FileEvent::FileType::jasp;
	else if (path.endsWith(".html", Qt::CaseInsensitive)) filetype = FileEvent::FileType::html;
	else if (path.endsWith(".csv", Qt::CaseInsensitive)) filetype = FileEvent::FileType::csv;
	else if (path.endsWith(".txt", Qt::CaseInsensitive)) filetype =  FileEvent::FileType::txt;
	else if (path.endsWith(".pdf", Qt::CaseInsensitive)) filetype =  FileEvent::FileType::pdf;

	if (filetype == FileEvent::FileType::unknown)
	{
		int dotPos = path.lastIndexOf('.');
		if (dotPos == -1 )
			filetype =  FileEvent::FileType::empty;
	}

	return filetype;
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

FileEvent::FileType FileEvent::type() const
{
	return _type;
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
