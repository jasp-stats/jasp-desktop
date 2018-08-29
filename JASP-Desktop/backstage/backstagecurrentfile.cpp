#include "backstagecurrentfile.h"
#include "ui_backstageform.h"
#include "qutils.h"
#include <QQmlContext>
#include <QFileInfo>
#include <QDir>

BackstageCurrentFile::BackstageCurrentFile(QWidget *parent): BackstagePage(parent),
	ui(new Ui::BackstageForm)
{
	ui->setupUi(this);
	
	_currentFileListModel = new CurrentFileListModel(this);
	
	ui->QmlContent->rootContext()->setContextProperty("backstageCurrentFile",this);
	ui->QmlContent->rootContext()->setContextProperty("currentFileListModel",_currentFileListModel);
	ui->QmlContent->setSource(QUrl(QStringLiteral("qrc:/backstage/BackstageCurrentFile.qml")));
	
	_currentFilePath = "";
	_currentDataFilePath = "";
	_currentFileType = Utils::FileType::unknown;
	_currentFileReadOnly = false;
	
	_currentFileListModel->setCurrentFilePath(_currentFilePath);
	
}

BackstageCurrentFile::~BackstageCurrentFile()
{
	delete ui;
}

void BackstageCurrentFile::setCurrentFilePath(const QString &path)
{
	
	_currentFilePath = path;
	_currentFileListModel->setCurrentFilePath(_currentFilePath);
	
}

void BackstageCurrentFile::setCurrentDataFilePath(const QString &path)
{
	_currentDataFilePath = path;
}

void BackstageCurrentFile::setCurrentFileType(const Utils::FileType &type)
{
	_currentFileType = type;
}

Utils::FileType BackstageCurrentFile::getCurrentFileType()
{
		return _currentFileType;
}

void BackstageCurrentFile::setCurrentFileReadOnly(const bool &readonly)
{
	_currentFileReadOnly = readonly;
}

bool BackstageCurrentFile::isCurrentFileReadOnly()
{
	return _currentFileReadOnly;
}

bool BackstageCurrentFile::isOnlineFile(const QString &path)
{
	return path.startsWith("http");
}

void BackstageCurrentFile::setCurrentFileInfo(const QString &path, const Utils::FileType &type, const bool &readonly)
{
	_currentFilePath = path;
	_currentFileType = type;
	_currentFileReadOnly = readonly;
	_currentFileListModel->setCurrentFilePath(_currentFilePath);
}

CurrentFileListModel *BackstageCurrentFile::getCurrentFileListModel()
{
	return _currentFileListModel;
}

// Slots 

QString BackstageCurrentFile::getCurrentFilePath()
{
	return _currentFilePath;
}

QString BackstageCurrentFile::getCurrentDataFilePath()
{
	return _currentDataFilePath;
	
}

QString BackstageCurrentFile::getCurrentDataFileName()
{
	QFileInfo  fi(_currentDataFilePath);
	return fi.fileName();	
}

QString BackstageCurrentFile::getCurrentDataFolder()
{
	QFileInfo  fi(_currentDataFilePath);
	return fi.path() + QDir::separator();
}

QString BackstageCurrentFile::getHeaderText()
{
	return QString("Double-click on the file below to synchronize or use " + getShortCutKey() + "-Y");
}


void BackstageCurrentFile::syncFile(FileEvent *event)
{
	emit dataSetIORequest(event);
}
