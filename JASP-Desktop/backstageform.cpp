#include "backstageform.h"
#include "ui_backstageform.h"

#include <iostream>
#include <fstream>
#include <QFileDialog>
#include <QStandardPaths>

#include <QDebug>

#include "../JASP-Common/datasetloader.h"

using namespace std;

BackStageForm::BackStageForm(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::BackStageForm)
{
    ui->setupUi(this);
}

BackStageForm::~BackStageForm()
{
    delete ui;
}


void BackStageForm::fileItemSelected()
{
    QString fileName = QFileDialog::getOpenFileName(this, tr("Open CSV File"), QStandardPaths::standardLocations(QStandardPaths::DocumentsLocation).first(), tr("CSV Files (*.csv)"));

    if ( ! fileName.isNull()) {

        ifstream is;
        is.open(fileName.toStdString().c_str(), ios::in);

        DataSet* dataSet = DataSetLoader::loadFile(is);

        emit dataSetLoaded(dataSet);
    }
}


