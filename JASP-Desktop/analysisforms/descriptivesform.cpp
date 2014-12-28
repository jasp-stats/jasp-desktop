#include "descriptivesform.h"
#include "ui_descriptivesform.h"

#include <boost/foreach.hpp>

#include <QDebug>

DescriptivesForm::DescriptivesForm(QWidget *parent) :
	AnalysisForm("DescriptivesForm", parent),
	ui(new Ui::DescriptivesForm)
{
	ui->setupUi(this);

	ui->listAvailableFields->setModel(&_availableVariablesModel);
	ui->listAvailableFields->setDoubleClickTarget(ui->main_fields);

	TableModelVariablesAssigned *model = new TableModelVariablesAssigned(this);
	model->setSource(&_availableVariablesModel);

	ui->main_fields->setModel(model);
	ui->main_fields->setDoubleClickTarget(ui->listAvailableFields);

	ui->buttonAssign_main_fields->setSourceAndTarget(ui->listAvailableFields, ui->main_fields);

	ui->pageStatistics->hide();
	ui->pageCharts->hide();
	ui->pageFormat->hide();

#ifdef QT_NO_DEBUG
	// temporary hides until the appropriate R code is implemented

	ui->widgetCharts->hide();
	ui->widgetFormat->hide();
	ui->statistics_valuesAreGroupMidpoints->hide();
	ui->plots->hide();
#else
	ui->widgetCharts->setStyleSheet("background-color: pink;");
	ui->widgetFormat->setStyleSheet("background-color: pink;");
	ui->statistics_valuesAreGroupMidpoints->setStyleSheet("background-color: pink;");
	ui->plots->setStyleSheet("background-color: pink;");
#endif

}

DescriptivesForm::~DescriptivesForm()
{
    delete ui;
}

