#include "bffromtform.h"
#include "ui_bffromtform.h"

BFFromTForm::BFFromTForm(QWidget *parent) :
	AnalysisForm("BFFromTForm", parent),
	ui(new Ui::BFFromTForm)
{
	ui->setupUi(this);
}

BFFromTForm::~BFFromTForm()
{
	delete ui;
}
