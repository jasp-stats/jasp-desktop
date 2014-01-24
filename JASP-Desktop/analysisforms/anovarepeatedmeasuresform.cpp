#include "anovarepeatedmeasuresform.h"
#include "ui_anovarepeatedmeasuresform.h"

AnovaRepeatedMeasuresForm::AnovaRepeatedMeasuresForm(QWidget *parent) :
	QWidget(parent),
	ui(new Ui::AnovaRepeatedMeasuresForm)
{
	ui->setupUi(this);
}

AnovaRepeatedMeasuresForm::~AnovaRepeatedMeasuresForm()
{
	delete ui;
}
