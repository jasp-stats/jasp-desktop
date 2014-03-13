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
	QString filename = QFileDialog::getOpenFileName(this, tr("Open CSV File"), QStandardPaths::standardLocations(QStandardPaths::DocumentsLocation).first(), tr("CSV Files (*.csv)"));

	if ( ! filename.isNull())
		emit dataSetSelected(filename);
}



