#include "bffromtpairedsamplesform.h"
#include "ui_bffromtpairedsamplesform.h"

BFFromTPairedSamplesForm::BFFromTPairedSamplesForm(QWidget *parent) :
	AnalysisForm("BFFromTPairedSamplesForm", parent),
	ui(new Ui::BFFromTPairedSamplesForm)
{
	ui->setupUi(this);
}

BFFromTPairedSamplesForm::~BFFromTPairedSamplesForm()
{
	delete ui;
}
