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
	_last_error = "Unkown error";
	switch (fileMode)
	{
	case FileEvent::FileExportResults:
		_exporter = new ResultExporter();
		break;
	case FileEvent::FileExportData:
		_exporter = new DataExporter();
		break;
	case FileEvent::FileSave:
		_exporter = new JASPExporter();
		break;
	default:
		_exporter = NULL;
	}
}

FileEvent::~FileEvent()
{
	if (_exporter != NULL) {
		delete _exporter;
	}
}

void FileEvent::setDataFilePath(const QString &path)
{
	_dataFilePath = path;
}

bool FileEvent::setPath(const QString &path)
{
	_path = path;
	bool result = true;
	Utils::FileType filetype = Utils::getTypeFromFileName(path.toStdString());


    if (filetype == Utils::unknown)
	{
		if (_exporter != NULL) {
			filetype = _exporter->getDefaultFileType();
			_path.append('.');
			_path.append(Utils::getFileTypeString(filetype));
		}
	}

	if (_exporter != NULL)
	{
		result = _exporter->isFileTypeAllowed(filetype);
		if (result)
		{
			_exporter->setFileType(filetype);
		}
		else
		{
			_last_error = "File must be of type ";
			bool first = true;
			bool last = false;
			Utils::FileTypeVector allowedFileTypes = _exporter->getAllowedFileTypes();
			for (Utils::FileTypeVector::const_iterator it = allowedFileTypes.begin(); it != allowedFileTypes.end(); it++) {
				Utils::FileTypeVector::const_iterator next = it; next++;
				if (next == allowedFileTypes.end()) last = true;
				if (last && !first) _last_error.append(" or ");
				if (!last && !first) _last_error.append(", ");
				_last_error.append(Utils::getFileTypeString(*it));
				first = false;
			}
		}
	}

	_type = filetype;

	return result;

}

QString FileEvent::getLastError() const {
	return _last_error;
}

void FileEvent::setReadOnly()
{
	_readOnly = true;
}

FileEvent::FileMode FileEvent::operation() const
{
	return _operation;
}

Utils::FileType FileEvent::type() const
{
	return _type;
}


const QString &FileEvent::path() const
{
	return _path;
}

const QString &FileEvent::dataFilePath() const
{
	return _dataFilePath;
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

void FileEvent::chainedComplete(FileEvent *event)
{
	setComplete(event->successful(), event->message());
}

bool FileEvent::IsOnlineNode() const
{
	return _path.startsWith("http");
}
