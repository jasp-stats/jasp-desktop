#include "backstagerecentfiles.h"
#include "ui_backstageform.h"


BackstageRecentFiles::BackstageRecentFiles(QWidget *parent): BackstagePage(parent),
	ui(new Ui::BackstageForm)
{
	
	ui->setupUi(this);

	_recentFilesListModel = new RecentFilesListModel(this);
	
	ui->QmlContent->rootContext()->setContextProperty("recentFilesListModel",_recentFilesListModel);
	ui->QmlContent->setSource(QUrl(QStringLiteral("qrc:/backstage/BackstageRecentFiles.qml")));
	
}

BackstageRecentFiles::~BackstageRecentFiles()
{
	delete ui;
}

void BackstageRecentFiles::pushRecentFilePath(const QString &newrecent)
{
	_recentFilesListModel->addRecentFilePath(newrecent);
}

void BackstageRecentFiles::openFile(FileEvent *event)
{
	emit dataSetIORequest(event);
}

