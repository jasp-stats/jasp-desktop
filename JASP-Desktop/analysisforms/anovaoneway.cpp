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
}

AnovaOneWay::~AnovaOneWay()
{
	delete ui;
}
