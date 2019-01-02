#include "currentfile.h"
#include "utilities/qutils.h"
#include <QQmlContext>
#include <QQmlEngine>
#include <QFileInfo>
#include <QDir>

CurrentFile::CurrentFile(QObject *parent): FileMenuObject(parent)
{	
	setListModel(new CurrentFileListModel(this));
	
	_currentFilePath = "";
	_currentDataFilePath = "";
	_currentFileType = Utils::FileType::unknown;
	_currentFileReadOnly = false;
	
	_currentFileListModel->setCurrentFilePath(_currentFilePath);
	
}

CurrentFile::~CurrentFile()
{
}

void CurrentFile::setCurrentFilePath(const QString &path)
{
	
	_currentFilePath = path;
	_currentFileListModel->setCurrentFilePath(_currentFilePath);
	
}

void CurrentFile::setCurrentDataFilePath(const QString &path)
{
	_currentDataFilePath = path;
}

void CurrentFile::setCurrentFileType(const Utils::FileType &type)
{
	_currentFileType = type;
}

Utils::FileType CurrentFile::getCurrentFileType()
{
		return _currentFileType;
}

void CurrentFile::setCurrentFileReadOnly(const bool &readonly)
{
	_currentFileReadOnly = readonly;
}

bool CurrentFile::isCurrentFileReadOnly()
{
	return _currentFileReadOnly;
}

bool CurrentFile::isOnlineFile(const QString &path)
{
	return path.startsWith("http");
}

void CurrentFile::setCurrentFileInfo(const QString &path, const Utils::FileType &type, const bool &readonly)
{
	_currentFilePath = path;
	_currentFileType = type;
	_currentFileReadOnly = readonly;
	_currentFileListModel->setCurrentFilePath(_currentFilePath);
}

CurrentFileListModel *CurrentFile::getCurrentFileListModel()
{
	return _currentFileListModel;
}

// Slots 

QString CurrentFile::getCurrentFilePath()
{
	return _currentFilePath;
}

QString CurrentFile::getCurrentDataFilePath()
{
	return _currentDataFilePath;
	
}

QString CurrentFile::getCurrentDataFileName()
{
	QFileInfo  fi(_currentDataFilePath);
	return fi.fileName();	
}

QString CurrentFile::getCurrentDataFolder()
{
	QFileInfo  fi(_currentDataFilePath);
	return fi.path() + QDir::separator();
}

QString CurrentFile::getHeaderText()
{
	return QString("Double-click on the file below to synchronize or use " + getShortCutKey() + "-Y");
}


void CurrentFile::syncFile(FileEvent *event)
{
	emit dataSetIORequest(event);
}


void CurrentFile::setListModel(CurrentFileListModel * listModel)
{
	if (_currentFileListModel == listModel)
		return;

	_currentFileListModel = listModel;
	emit listModelChanged(_currentFileListModel);
}
