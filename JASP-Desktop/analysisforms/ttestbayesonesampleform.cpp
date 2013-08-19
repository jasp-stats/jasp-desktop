#include "ttestbayesonesampleform.h"
#include "ui_ttestbayesonesampleform.h"

TTestBayesOneSampleForm::TTestBayesOneSampleForm(QWidget *parent) :
	AnalysisForm(parent),
	ui(new Ui::TTestBayesOneSampleForm)
{
	ui->setupUi(this);

	ui->listAvailableFields->setModel(&_availableFields);
	ui->listAvailableFields->addAssignButton(ui->buttonAssign_main_fields);

	ui->variables->setAssignButton(ui->buttonAssign_main_fields);
	ui->variables->setAvailableFieldsListView(ui->listAvailableFields);
}

TTestBayesOneSampleForm::~TTestBayesOneSampleForm()
{
	delete ui;
}
