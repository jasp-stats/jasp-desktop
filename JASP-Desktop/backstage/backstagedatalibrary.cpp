#include "backstagedatalibrary.h"
#include "ui_backstageform.h"

BackstageDataLibrary::BackstageDataLibrary(QWidget *parent) : BackstagePage(parent),
	ui(new Ui::BackstageForm)
{
	ui->setupUi(this);
	
	_dataLibraryBreadCrumbsListModel = new DataLibraryBreadCrumbsListModel(this);
	_dataLibraryBreadCrumbsListModel->setSeparator(QDir::separator());
	
	_dataLibraryListModel = new DataLibraryListModel(this);
	_dataLibraryListModel->setBreadCrumbsListModel(_dataLibraryBreadCrumbsListModel);
	
	connect(_dataLibraryBreadCrumbsListModel, SIGNAL(crumbIndexChanged(const int &)), _dataLibraryListModel, SLOT(changePath(const int &)));
	
	ui->QmlContent->rootContext()->setContextProperty("dataLibraryListModel",_dataLibraryListModel);
	ui->QmlContent->rootContext()->setContextProperty("breadcrumbsmodel",_dataLibraryBreadCrumbsListModel); // Calling changePath(index)
	ui->QmlContent->rootContext()->setContextProperty("dataLibraryBreadCrumbsListModel",_dataLibraryBreadCrumbsListModel);
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


