#include "backstageform.h"
#include "ui_backstageform.h"

#include <QFileDialog>
#include <QStandardPaths>

using namespace std;

BackStageForm::BackStageForm(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::BackStageForm)
{
    ui->setupUi(this);

	connect(ui->buttonOpen, SIGNAL(clicked()), this, SLOT(fileItemSelected()));
	connect(ui->buttonClose, SIGNAL(clicked()), this, SLOT(closeItemSelected()));
	connect(ui->buttonExport, SIGNAL(clicked()), this, SLOT(exportItemSelected()));

	setFileLoaded(false);
}

BackStageForm::~BackStageForm()
{
	delete ui;
}

void BackStageForm::setFileLoaded(bool loaded)
{
	ui->buttonClose->setEnabled(loaded);
	ui->buttonExport->setEnabled(loaded);
}

void BackStageForm::fileItemSelected()
{
	_settings.sync();
	QString path = _settings.value("openPath", QStandardPaths::standardLocations(QStandardPaths::DocumentsLocation).first()).toString();
	QString filename = QFileDialog::getOpenFileName(this, tr("Open CSV File"), path, tr("CSV Files (*.csv)"));

	if ( ! filename.isNull())
	{
		QFileInfo f(filename);
		path = f.absolutePath();
		_settings.setValue("openPath", path);
		_settings.sync();

		emit dataSetSelected(filename);
	}
}

void BackStageForm::closeItemSelected()
{
	emit closeDataSetSelected();
}

void BackStageForm::exportItemSelected()
{
	_settings.sync();
	QString path = _settings.value("openPath", QStandardPaths::standardLocations(QStandardPaths::DocumentsLocation).first()).toString() + QDir::separator() + "Results.html";
	QString filename = QFileDialog::getSaveFileName(this, tr("Export as HTML"), path, tr("HTML Files (*.html)"));

	if ( ! filename.isNull())
	{
		QFileInfo f(filename);
		path = f.absolutePath();
		_settings.setValue("openPath", path);
		_settings.sync();

		emit exportSelected(filename);
	}
}



