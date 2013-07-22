#include "ttestonesample.h"
#include "ui_ttestonesample.h"

#include "analysisform.h"

TTestOneSample::TTestOneSample(QWidget *parent) :
	AnalysisForm(parent),
	ui(new Ui::TTestOneSample)
{
	ui->setupUi(this);

	ui->listAvailableFields->setModel(&_availableFields);
	ui->listAvailableFields->addAssignButton(ui->buttonAssign_main_fields);

	ui->variables->setAssignButton(ui->buttonAssign_main_fields);
	ui->variables->setAvailableFieldsListView(ui->listAvailableFields);
}

TTestOneSample::~TTestOneSample()
{
	delete ui;
}
