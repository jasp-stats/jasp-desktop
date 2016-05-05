#include "bffromtonesampleform.h"
#include "ui_bffromtonesampleform.h"

BFFromTOneSampleForm::BFFromTOneSampleForm(QWidget *parent) :
	AnalysisForm("BFFromTOneSampleForm", parent),
	ui(new Ui::BFFromTOneSampleForm)
{
	ui->setupUi(this);
}

BFFromTOneSampleForm::~BFFromTOneSampleForm()
{
	delete ui;
}
