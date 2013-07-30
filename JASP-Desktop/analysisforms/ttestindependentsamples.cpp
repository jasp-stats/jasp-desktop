#include "ttestindependentsamples.h"
#include "ui_ttestindependentsamples.h"

#include "analysisform.h"

TTestIndependentSamples::TTestIndependentSamples(QWidget *parent) :
	AnalysisForm(parent),
	ui(new Ui::TTestIndependentSamples)
{
	ui->setupUi(this);

	ui->listAvailableFields->setModel(&_availableFields);
	ui->listAvailableFields->addAssignButton(ui->buttonAssignVariables);
	ui->listAvailableFields->addAssignButton(ui->buttonAssignGroupingVariable);

	ui->variables->setAssignButton(ui->buttonAssignVariables);
	ui->variables->setAvailableFieldsListView(ui->listAvailableFields);

	ui->groupingVariable->setAssignButton(ui->buttonAssignGroupingVariable);
	ui->groupingVariable->setAvailableFieldsListView(ui->listAvailableFields);
}

TTestIndependentSamples::~TTestIndependentSamples()
{
	delete ui;
}
