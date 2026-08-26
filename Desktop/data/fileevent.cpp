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
#include "exporters/exporter.h"
#include "exporters/dataexporter.h"
#include "exporters/resultexporter.h"
#include "exporters/jaspexporter.h"
#include "dataset.h"
#include "datasetpackage.h"
#include "log.h"

#include <QTimer>
#include "fileevent.h"
#include "processinfo.h"
#include "utilities/appdirs.h"
#include "exporters/dataexporter.h"
#include "exporters/jaspexporter.h"
#include "exporters/resultexporter.h"
#include "widgets/filemenu/filemenu.h"
#include "log.h"

void FileEvent::setDataSet(DataSet * ds)
{ 
	_dataSet = ds;
	_dataSetId = ds ? ds->id() : -1; //snapshot now, while ds is still alive
	Log::log() << "[FileEvent::setDataSet] Set dataSet to: " << (ds ? QString::number(ds->id()) : "NULL") << std::endl;
}

DataSet * FileEvent::dataSet() const
{
	return _dataSet;
}

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

	// The FileMenu handles the started & completed signals
	connect(this, &FileEvent::started,		FileMenu::singleton(), &FileMenu::startFileEvent);
	connect(this, &FileEvent::completed,	FileMenu::singleton(), &FileMenu::finalizeFileEvent, Qt::QueuedConnection);

}

FileEvent::~FileEvent()
{
	   delete _exporter;
	   _exporter = nullptr;
}

void FileEvent::setDataFilePath(const QString & path)
{
	_dataFilePath = path;
}

void FileEvent::setDatabase(const Json::Value & dbInfo)
{
	_database = dbInfo;
	Log::log() << "[FileEvent::setDatabase] Database set" << std::endl;
}

bool FileEvent::setPath(const QString & path)
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

void FileEvent::starts()
{
	Log::log() << "File Event Starts: " << getProgressMsg().toStdString() << std::endl;

	if (isStarted())
	{
		Log::log() << "Try to start event '" << getProgressMsg().toStdString() << "', but it was already started!" << std::endl;
		return;
	}

	_status = EventStatus::EventStarted;

	emit started();
}

void FileEvent::setComplete(bool success, const QString & message, bool cancelled)
{
	Log::log() << "File Event Completed: " << getProgressMsg().toStdString() << std::endl;

	if (isCompleted())
	{
		Log::log() << "Try to set complete event '" << getProgressMsg().toStdString() << "', but it was already completed!" << std::endl;
		return;
	}
	_status     = EventStatus::EventCompleted;
	_success	= success;
	_cancelled	= cancelled;
	_message	= message;

	Log::log() << "[FileEvent::setComplete] operation=" << _operation << ", success=" << success << ", message=" << message.toStdString() << std::endl;

	emit completed();
}

void FileEvent::cleanUp()
{
	emit finalized();
	deleteLater();
}


void FileEvent::chain(FileEvent *event, bool resetDataSet)
{
	if (isStarted())
	{
		if (isCompleted())
			Log::log() << "Event " << getProgressMsg().toStdString() << " is completed before Event " << event->getProgressMsg().toStdString() << " was completed" << std::endl;
		else // The `this` event is already started, but it should wait for `event`to be finalized before being set to be completed
			connect(event, &FileEvent::finalized, this, [this, event]() { setComplete(event->isSuccessful(), event->message()); });
	}
	else
		connect(event, &FileEvent::finalized, this, [this, event, resetDataSet]() {
			if (event->isSuccessful())
			{
				if (resetDataSet && DataSetPackage::pkg()->hasDataSet())
					setDataSet(DataSetPackage::pkg()->dataSet());
				starts();
			}
			else
				setComplete(false, event->message(), event->isCancelled());
		} );
}

bool FileEvent::isExample() const
{
	return path().startsWith(AppDirs::examples());
}

void FileEvent::setSilent(bool newSilent)
{
	_cancelled = newSilent;
}

bool FileEvent::autoSaveExists()
{
	return QFileInfo::exists(pathTmp());
}

void FileEvent::removeAutoSaveIfItExists()
{
	if(!autoSaveExists())
		return;
	
	QFile autoSaveFile(pathTmp());
	
	autoSaveFile.remove();
}

QString FileEvent::pathTmp()
{
	static QString tmpFile = []()
	{
		QDir autoSaveFolder(AppDirs::autoSaveDir());
		
		QFileInfoList files = autoSaveFolder.entryInfoList(QDir::Filter::Files | QDir::NoDotAndDotDot | QDir::NoSymLinks);
		
		std::set<QString> usedNames;
		
		for(QFileInfo & fi : files)
			usedNames.insert(fi.fileName());
		
		auto aNamePlease = [](){
			static int num = 0;
			return "autosave-" + QString::number(ProcessInfo::currentPID()) + "-" + QString::number(num++) + ".jasp";
		};
		
		QString newFileName;
		
		do		{ newFileName = aNamePlease();	}
		while	( usedNames.count(newFileName)	);
		
		return autoSaveFolder.absoluteFilePath(newFileName);
		
	}();
	
	return tmpFile;
}

const std::string FileEvent::databaseStr() const 
{ 
	return _database.toStyledString();
}

QString FileEvent::getProgressMsg() const
{
	//jasp = 0, html, csv, txt, tsv, sav, zsav, ods, xls, xlsx, pdf, sas7bdat, sas7bcat, por, xpt, dta, database, rdata, rds, empty, unknown
	switch(_operation)
	{
	case FileEvent::FileOpen:
		switch(_type)
		{
		case Utils::FileType::csv:
		case Utils::FileType::txt:
		case Utils::FileType::tsv:
		case Utils::FileType::ods:			return tr("Importing Data from %1").arg(FileTypeBaseToQString(_type).toUpper());
		case Utils::FileType::xls:
		case Utils::FileType::xlsx:			return tr("Importing Excel File");
        case Utils::FileType::sav:
		case Utils::FileType::zsav:
		case Utils::FileType::por:			return tr("Importing SPSS File");
		case Utils::FileType::xpt:
		case Utils::FileType::sas7bdat:
		case Utils::FileType::sas7bcat:		return tr("Importing SAS File");
		case Utils::FileType::dta:			return tr("Importing STATA File");
		case Utils::FileType::jasp:			return tr("Loading JASP File");
		case Utils::FileType::rdata:
		case Utils::FileType::rds:			return tr("Loading R Data File");
		case Utils::FileType::mwx:
		case Utils::FileType::mpx:			return tr("Loading Minitab Data Workbook File");
		default:							return tr("Loading File");
		}
		break;

	case FileEvent::FileSave:			return !_tmp ? tr("Saving JASP File") : tr("Saving autosave JASP File");
	case FileEvent::FileExportResults:	return tr("Exporting Results");
	case FileEvent::FileExportData:
	case FileEvent::FileGenerateData:	return tr("Exporting Data");
	default:							break;
	}

	return tr("Processing File"); //This will never show up on screen right?
}
