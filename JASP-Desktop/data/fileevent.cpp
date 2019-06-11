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

#include "fileevent.h"
#include "exporters/dataexporter.h"
#include "exporters/resultexporter.h"
#include "exporters/jaspexporter.h"

#include <QTimer>

FileEvent::FileEvent(QObject *parent, FileEvent::FileMode fileMode)
	: QObject(parent), _operation(fileMode)
{
	switch (_operation)
	{
	case FileEvent::FileExportResults:	_exporter = new ResultExporter();		break;
	case FileEvent::FileExportData:		_exporter = new DataExporter(true);		break;
	case FileEvent::FileGenerateData:	_exporter = new DataExporter(false);	break;
	case FileEvent::FileSave:			_exporter = new JASPExporter();			break;
	default:							_exporter = nullptr;					break;
	}
}

FileEvent::~FileEvent()
{
	delete _exporter;
	_exporter = nullptr;
}

void FileEvent::setDataFilePath(const QString &path)
{
	_dataFilePath = path;
}

bool FileEvent::setPath(const QString &path)
{
	_path = path;
	_type = Utils::getTypeFromFileName(path.toStdString());

	if(_exporter != nullptr)
	{
		if (_type == Utils::unknown)
		{
			_type = _exporter->getDefaultFileType();
			_path.append('.' + QString(Utils::getFileTypeString(_type)));
		}
		else if(!_exporter->isFileTypeAllowed(_type)) //Because an exporter should always support it's own default
		{
			_last_error = "File must be of type ";

			Utils::FileTypeVector allowedFileTypes = _exporter->getAllowedFileTypes();

			for (size_t i=0; i< allowedFileTypes.size(); i++)
			{
				if(i > 0)
				{
					if (i == allowedFileTypes.size() - 1)	_last_error.append(" or ");
					else									_last_error.append(", ");
				}

				_last_error.append(Utils::getFileTypeString(allowedFileTypes[i]));
			}

			return false;
		}

		_exporter->setFileType(_type);
	}

	return true;

}

void FileEvent::setComplete(bool success, const QString & message)
{
	_completed	= true;
	_success	= success;
	_message	= message;

	emit completed(this);
}

void FileEvent::chain(FileEvent *event)
{
	_chainedTo = event;
	connect(event, &FileEvent::completed, this, &FileEvent::chainedComplete);
}

