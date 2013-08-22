#include "anovamultivariateform.h"
#include "ui_anovamultivariateform.h"

AnovaMultivariateForm::AnovaMultivariateForm(QWidget *parent) :
	AnalysisForm(parent),
	ui(new Ui::AnovaMultivariateForm)
{
	ui->setupUi(this);
}

AnovaMultivariateForm::~AnovaMultivariateForm()
{
	delete ui;
}
