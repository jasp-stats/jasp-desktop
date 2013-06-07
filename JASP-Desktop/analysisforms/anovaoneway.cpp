#include "anovaoneway.h"
#include "ui_anovaoneway.h"

#include "analysisform.h"

#include "boost/foreach.hpp"

ANOVAOneWay::ANOVAOneWay(QWidget *parent) :
	AnalysisForm(parent),
	ui(new Ui::ANOVAOneWay)
{
	ui->setupUi(this);

	ui->listAvailableFields->setModel(&_availableFields);
	ui->listAvailableFields->addAssignButton(ui->buttonAssign_main_fields);
	ui->listAvailableFields->addAssignButton(ui->buttonAssign_main_factor);

	ui->main_fields->setAssignButton(ui->buttonAssign_main_fields);
	ui->main_fields->setAvailableFieldsListView(ui->listAvailableFields);

	ui->main_factor->setAssignButton(ui->buttonAssign_main_factor);
	ui->main_factor->setAvailableFieldsListView(ui->listAvailableFields);

	connect(ui->okButton, SIGNAL(clicked()), this, SLOT(accept()));
}

ANOVAOneWay::~ANOVAOneWay()
{
	delete ui;
}
