#include "descriptives.h"
#include "ui_descriptives.h"

#include <boost/foreach.hpp>

#include <QDebug>

Descriptives::Descriptives(QWidget *parent) :
	AnalysisForm(parent),
	ui(new Ui::Descriptives)
{
	ui->setupUi(this);

	ui->listAvailableFields->setModel(&_availableFields);
	ui->listAvailableFields->setDoubleClickTarget(ui->main_fields);

	ui->main_fields->setModel(new ListModelVariablesAssigned(this));
	ui->main_fields->setDoubleClickTarget(ui->listAvailableFields);

	ui->buttonAssign_main_fields->setSourceAndTarget(ui->listAvailableFields, ui->main_fields);

	ui->pageStatistics->hide();
	ui->pageCharts->hide();
	ui->pageFormat->hide();
}

Descriptives::~Descriptives()
{
    delete ui;
}

