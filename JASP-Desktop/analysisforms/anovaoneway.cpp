#include "anovaoneway.h"
#include "ui_anovaoneway.h"

#include "analysisform.h"

#include "boost/foreach.hpp"

AnovaOneWay::AnovaOneWay(QWidget *parent) :
	AnalysisForm(parent),
	ui(new Ui::ANOVAOneWay)
{
	ui->setupUi(this);

	ui->listAvailableFields->setModel(&_availableFields);
	ui->listAvailableFields->addAssignButton(ui->assignFactor);
	ui->listAvailableFields->addAssignButton(ui->assignVariables);

	ui->variables->setAssignButton(ui->assignVariables);
	ui->variables->setAvailableFieldsListView(ui->listAvailableFields);

	ui->factor->setAssignButton(ui->assignFactor);
	ui->factor->setAvailableFieldsListView(ui->listAvailableFields);
}

AnovaOneWay::~AnovaOneWay()
{
	delete ui;
}
