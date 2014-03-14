#include "semsimpleform.h"
#include "ui_semsimpleform.h"

SEMSimpleForm::SEMSimpleForm(QWidget *parent) :
	AnalysisForm("SEMSimpleForm", parent),
	ui(new Ui::SEMSimpleForm)
{
	ui->setupUi(this);

	ui->containerStatistics->hide();
	ui->containerOptions->hide();
	ui->containerAdvanced->hide();
}

SEMSimpleForm::~SEMSimpleForm()
{
	delete ui;
}
