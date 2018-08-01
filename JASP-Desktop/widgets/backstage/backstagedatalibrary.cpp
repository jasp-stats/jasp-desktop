#include "backstagedatalibrary.h"
#include "ui_backstagedatalibrary.h"

BackstageDataLibrary::BackstageDataLibrary(QWidget *parent) : BackstagePage(parent),
	ui(new Ui::BackstageDataLibrary)
{
	ui->setupUi(this);
	
	_dataLibraryBreadCrumbsModel = new DataLibraryBreadCrumbsModel(this);
	_dataLibraryBreadCrumbsModel->setSeperator(QDir::separator());
	
	_dataLibraryListModel = new DataLibraryListModel(this);
	_dataLibraryListModel->setDataLibraryBreadCrumbsModel(_dataLibraryBreadCrumbsModel);
	
	ui->QmlContent->rootContext()->setContextProperty("dataLibraryListModel",_dataLibraryListModel);
	ui->QmlContent->rootContext()->setContextProperty("dataLibraryBreadCrumbsModel",_dataLibraryBreadCrumbsModel);
	ui->QmlContent->rootContext()->setContextProperty("backstagedatalibrary",this);		
	ui->QmlContent->setSource(QUrl(QStringLiteral("qrc:/backstage/BackstageDataLibrary.qml")));
}

BackstageDataLibrary::~BackstageDataLibrary()
{
	delete ui;
}

void BackstageDataLibrary::openFile(FileEvent *event)
{
	emit dataSetIORequest(event);
}


