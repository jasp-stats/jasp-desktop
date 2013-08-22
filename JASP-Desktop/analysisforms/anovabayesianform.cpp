#include "anovabayesianform.h"
#include "ui_anovabayesianform.h"

AnovaBayesianForm::AnovaBayesianForm(QWidget *parent) :
	AnalysisForm(parent),
	ui(new Ui::AnovaBayesianForm)
{
	ui->setupUi(this);
}

AnovaBayesianForm::~AnovaBayesianForm()
{
	delete ui;
}
