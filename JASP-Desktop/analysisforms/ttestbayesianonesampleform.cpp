#include "ttestbayesianonesampleform.h"
#include "ui_ttestbayesonesampleform.h"

TTestBayesianOneSampleForm::TTestBayesianOneSampleForm(QWidget *parent) :
	AnalysisForm(parent),
	ui(new Ui::TTestBayesOneSampleForm)
{
	ui->setupUi(this);

	ui->listAvailableFields->setModel(&_availableFields);
	ui->listAvailableFields->addAssignButton(ui->buttonAssign_main_fields);

	ui->variables->setAssignButton(ui->buttonAssign_main_fields);
	ui->variables->setAvailableFieldsListView(ui->listAvailableFields);
}

TTestBayesianOneSampleForm::~TTestBayesianOneSampleForm()
{
	delete ui;
}
