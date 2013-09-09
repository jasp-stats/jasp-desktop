#include "ttestbayesianonesampleform.h"
#include "ui_ttestbayesianonesampleform.h"

TTestBayesianOneSampleForm::TTestBayesianOneSampleForm(QWidget *parent) :
	AnalysisForm(parent),
	ui(new Ui::TTestBayesianOneSampleForm)
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
