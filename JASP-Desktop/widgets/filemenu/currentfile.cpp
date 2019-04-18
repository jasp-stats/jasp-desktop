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

bool CurrentFile::isOnlineFile(const QString &path)
{
	return path.startsWith("http");
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

QString CurrentFile::getHeaderText()
{
	return QString("Click on the file below to synchronize or use " + getShortCutKey() + "-Y");
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
	connect(_currentFileListModel, &CurrentFileListModel::syncCurrentFile, this, &CurrentFile::syncFile);

	emit listModelChanged(_currentFileListModel);
}
