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
	ui->listAvailableFields->addAssignButton(ui->buttonAssign_main_fields);

	ui->main_fields->setAssignButton(ui->buttonAssign_main_fields);
	ui->main_fields->setAvailableFieldsListView(ui->listAvailableFields);

	ui->pageStatistics->hide();
	ui->pageCharts->hide();
	ui->pageFormat->hide();
}

Descriptives::~Descriptives()
{
    delete ui;
}

