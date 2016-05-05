#include "bffromtindependentsamplesform.h"
#include "ui_bffromtindependentsamplesform.h"

BFFromTIndependentSamplesForm::BFFromTIndependentSamplesForm(QWidget *parent) :
	AnalysisForm("BFFromTIndependentSamplesForm", parent),
	ui(new Ui::BFFromTIndependentSamplesForm)
{
	ui->setupUi(this);
}

BFFromTIndependentSamplesForm::~BFFromTIndependentSamplesForm()
{
	delete ui;
}
