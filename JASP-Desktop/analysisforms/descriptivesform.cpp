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
	ui->listAvailableFields->setDoubleClickTarget(ui->variables);

	TableModelVariablesAssigned *model = new TableModelVariablesAssigned(this);
	model->setSource(&_availableVariablesModel);

	ui->variables->setModel(model);
	ui->variables->setDoubleClickTarget(ui->listAvailableFields);

	ui->buttonAssign_main_fields->setSourceAndTarget(ui->listAvailableFields, ui->variables);

	ui->pageStatistics->hide();
	ui->pageCharts->hide();
	ui->pageFormat->hide();

#ifdef QT_NO_DEBUG
	// temporary hides until the appropriate R code is implemented

	ui->widgetCharts->hide();
	ui->widgetFormat->hide();
	ui->statisticsValuesAreGroupMidpoints->hide();
#else
	ui->widgetCharts->setStyleSheet("background-color: pink;");
	ui->widgetFormat->setStyleSheet("background-color: pink;");
	ui->statisticsValuesAreGroupMidpoints->setStyleSheet("background-color: pink;");
#endif

}

DescriptivesForm::~DescriptivesForm()
{
    delete ui;
}

