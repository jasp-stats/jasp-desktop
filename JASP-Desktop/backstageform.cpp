#include "backstageform.h"
#include "ui_backstageform.h"

#include <iostream>
#include <fstream>
#include <QFileDialog>
#include <QStandardPaths>

#include <QDebug>

#include "../JASP-Common/datasetloader.h"
#include "asyncloader.h"

using namespace std;

BackStageForm::BackStageForm(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::BackStageForm)
{
    ui->setupUi(this);

	connect(ui->buttonOpen, SIGNAL(clicked()), this, SLOT(fileItemSelected()));
}

BackStageForm::~BackStageForm()
{
    delete ui;
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



