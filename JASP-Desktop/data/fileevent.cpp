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
#include "utilities/qutils.h"

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
		if (_type == Utils::FileType::unknown)
		{
			_type = _exporter->getDefaultFileType();
			_path.append('.' + FileTypeBaseToQString(_type));
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

				_last_error.append(FileTypeBaseToQString(allowedFileTypes[i]));
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

QString FileEvent::getProgressMsg() const
{
	//jasp = 0, html, csv, txt, tsv, sav, ods, pdf, sas7bdat, sas7bcat, por, xpt, empty, unknown
	switch(_operation)
	{
	case FileEvent::FileOpen:
		switch(_type)
		{
		case Utils::FileType::csv:
		case Utils::FileType::txt:
		case Utils::FileType::tsv:
		case Utils::FileType::ods:		return tr("Importing Data from %1").arg(FileTypeBaseToQString(_type).toUpper());
		case Utils::FileType::sav:
		case Utils::FileType::por:		return tr("Importing SPSS File");
		case Utils::FileType::xpt:
		case Utils::FileType::sas7bdat:
		case Utils::FileType::sas7bcat:	return tr("Importing SAS File");
		case Utils::FileType::dta:		return tr("Importing STATA File");
		case Utils::FileType::jasp:		return tr("Loading JASP File");
		default:						return tr("Loading File");
		}
		break;

	case FileEvent::FileSave:			return tr("Saving JASP File");
	case FileEvent::FileExportResults:	return tr("Exporting Results");
	case FileEvent::FileExportData:
	case FileEvent::FileGenerateData:	return tr("Exporting Data");
	case FileEvent::FileSyncData:		return tr("Synchronizing Data");
	default:							break;
	}

	return tr("Processing File"); //This will never show up on screen right?
}

