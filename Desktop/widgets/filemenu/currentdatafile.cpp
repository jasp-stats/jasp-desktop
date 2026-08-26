#include "currentdatafile.h"
#include "qutils.h"
#include <QQmlContext>
#include <QQmlEngine>
#include <QFileInfo>
#include <QDir>
#include "data/datasetpackage.h"

CurrentDataFile::CurrentDataFile(FileMenu *parent): FileMenuObject(parent)
{	
	setListModel(new CurrentFileListModel(this));
	
	_currentFilePath = "";
	_currentFileListModel->setCurrentFilePath(_currentFilePath);	
}

CurrentDataFile::~CurrentDataFile()
{
}

void CurrentDataFile::setCurrentFilePath(const QString &path)
{
	_currentFilePath = path;
	_currentFileListModel->setCurrentFilePath(_currentFilePath);
	
}

bool CurrentDataFile::isOnlineFile(const QString &path)
{
	return path.startsWith("http");
}

CurrentFileListModel *CurrentDataFile::getCurrentFileListModel()
{
	return _currentFileListModel;
}

// Slots 

QString CurrentDataFile::getCurrentFilePath()
{
	return _currentFilePath;
}

QString CurrentDataFile::getHeaderText()
{
	return QString(tr("Click on the file below to synchronize or use ") + getShortCutKey() + "-Y");
}


void CurrentDataFile::syncFile(const QString & path)
{
	emit setCheckAutomaticSync(false);
	FileEvent *event = new FileEvent(this, FileEvent::FileSyncData);
	event->setPath(path);
	if (DataSetPackage::pkg()->hasDataSet())
		event->setDataSet(DataSetPackage::pkg()->dataSet());

	event->starts();
}


void CurrentDataFile::setListModel(CurrentFileListModel * listModel)
{
	if (_currentFileListModel == listModel)
		return;

	_currentFileListModel = listModel;
	connect(_currentFileListModel, &CurrentFileListModel::syncCurrentFile, this, &CurrentDataFile::syncFile);

	emit listModelChanged(_currentFileListModel);
}
