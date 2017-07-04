//
// Copyright (C) 2017 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

#include "mlclassificationboostingform.h"
#include "ui_mlclassificationboostingform.h"

MLClassificationBoostingForm::MLClassificationBoostingForm(QWidget *parent) :
    AnalysisForm("MLClassificationBoostingForm", parent),
    ui(new Ui::MLClassificationBoostingForm)
{
    ui->setupUi(this);

    ui->listAvailableFields->setModel(&_availableVariablesModel);

    _targetListModel = new TableModelVariablesAssigned(this);
    _targetListModel->setSource(&_availableVariablesModel);
    _targetListModel->setVariableTypesSuggested(Column::ColumnTypeScale | Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
    ui->target->setModel(_targetListModel);

    _predictorsListModel = new TableModelVariablesAssigned(this);
    _predictorsListModel->setSource(&_availableVariablesModel);
    _predictorsListModel->setVariableTypesSuggested(Column::ColumnTypeScale | Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
    ui->predictors->setModel(_predictorsListModel);

    _indicatorListModel = new TableModelVariablesAssigned(this);
    _indicatorListModel->setSource(&_availableVariablesModel);
    _indicatorListModel->setVariableTypesSuggested(Column::ColumnTypeScale | Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);//Column::ColumnTypeScale
    //_indicatorListModel->setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
    ui->indicator->setModel(_indicatorListModel);

    ui->buttonAssignFixed->setSourceAndTarget(ui->listAvailableFields, ui->target);
    ui->buttonAssignRandom->setSourceAndTarget(ui->listAvailableFields, ui->predictors);
    ui->buttonAssignCovariates->setSourceAndTarget(ui->listAvailableFields, ui->indicator);

    connect(_targetListModel, SIGNAL(assignmentsChanging()), this, SLOT(factorsChanging()));
    connect(_targetListModel, SIGNAL(assignmentsChanged()), this, SLOT(factorsChanged()));

    connect(_predictorsListModel, SIGNAL(assignmentsChanging()), this, SLOT(factorsChanging()));
    connect(_predictorsListModel, SIGNAL(assignmentsChanged()), this, SLOT(factorsChanged()));

    connect(_indicatorListModel, SIGNAL(assignmentsChanging()), this, SLOT(factorsChanging()));
    connect(_indicatorListModel, SIGNAL(assignmentsChanged()), this, SLOT(factorsChanged()));

    ui->advancedOptions->hide();

    defaultOptions();
}

MLClassificationBoostingForm::~MLClassificationBoostingForm()
{
    delete ui;
}

void MLClassificationBoostingForm::defaultOptions()
{
    QSizePolicy retain = ui->value_subsample->sizePolicy();
    retain.setRetainSizeWhenHidden(true);

    ui->value_subsample->setSizePolicy(retain);
    ui->value_subsample->hide();

    ui->value_percentageTraining->setSizePolicy(retain);
    ui->value_percentageTraining->hide();

    ui->value_numberOfFold->setSizePolicy(retain);
    ui->value_numberOfFold->hide();

    ui->value_numberOfCore->setSizePolicy(retain);
    ui->value_numberOfCore->hide();

    ui->value_seed->setSizePolicy(retain);
    ui->value_seed->hide();

//    QSizePolicy retain2 = ui->value_depthOfTree->sizePolicy();
    retain.setRetainSizeWhenHidden(false);

    ui->value_depthOfTree->setSizePolicy(retain);
    ui->value_depthOfTree->hide();

    ui->value_MinTermNodeSide->setSizePolicy(retain);
    ui->value_MinTermNodeSide->hide();

    ui->value_depthOfTreeMin->setSizePolicy(retain);
    ui->value_depthOfTreeMin->hide();

    ui->value_depthOfTreeMax->setSizePolicy(retain);
    ui->value_depthOfTreeMax->hide();

    ui->value_depthOfTreeStep->setSizePolicy(retain);
    ui->value_depthOfTreeStep->hide();

    ui->value_MinTermNodeSideMin->setSizePolicy(retain);
    ui->value_MinTermNodeSideMin->hide();

    ui->value_MinTermNodeSideMax->setSizePolicy(retain);
    ui->value_MinTermNodeSideMax->hide();

    ui->value_MinTermNodeSideStep->setSizePolicy(retain);
    ui->value_MinTermNodeSideStep->hide();

//    retain3 = ui->label_9->sizePolicy();
    retain.setRetainSizeWhenHidden(true);

    ui->label_9->setSizePolicy(retain);
    ui->label_9->hide();

//    retain = ui->label_16->sizePolicy();
    retain.setRetainSizeWhenHidden(false);

    ui->label_16->setSizePolicy(retain);
    ui->label_16->hide();

    ui->label_20->setSizePolicy(retain);
    ui->label_20->hide();

    ui->label_8->setSizePolicy(retain);
    ui->label_8->hide();

    ui->label_12->setSizePolicy(retain);
    ui->label_12->hide();

    ui->label_7->setSizePolicy(retain);
    ui->label_7->hide();

    ui->label_10->setSizePolicy(retain);
    ui->label_10->hide();

    ui->label_11->setSizePolicy(retain);
    ui->label_11->hide();
}

void MLClassificationBoostingForm::bindTo(Options *options, DataSet *dataSet)
{
    AnalysisForm::bindTo(options, dataSet);

    factorsChanging();

    factorsChanged();
}

void MLClassificationBoostingForm::factorsChanging()
{
    if (_options != NULL)
        _options->blockSignals(true);
}

void MLClassificationBoostingForm::factorsChanged()
{
    if (_options != NULL)
        _options->blockSignals(false);
}

//void MLRegressionBoostingForm::on_seedAuto_clicked(bool checked)
//{
//        ui->plotMarginalPlotOneWay->setEnabled(true);
//}
void MLClassificationBoostingForm::on_manual_subsample_clicked(bool checked)
{
    if (checked) {
        ui->value_subsample->show();
    }
}

void MLClassificationBoostingForm::on_auto_subsample_clicked(bool checked)
{
    if (checked) {
        ui->value_subsample->hide();
    }
}

void MLClassificationBoostingForm::on_manual_percentageTrain_clicked(bool checked)
{
    if (checked) {
        ui->value_percentageTraining->show();
    }
}

void MLClassificationBoostingForm::on_auto_percentageTrain_clicked(bool checked)
{
    if (checked) {
        ui->value_percentageTraining->hide();
    }
}

void MLClassificationBoostingForm::on_methodCV_clicked(bool checked)
{
    if (checked) {
        ui->value_numberOfFold->show();
        ui->label_9->show();
    }
}

void MLClassificationBoostingForm::on_manual_numberOfCore_clicked(bool checked)
{
    if (checked) {
        ui->value_numberOfCore->show();
    }
}

void MLClassificationBoostingForm::on_auto_numberOfCore_clicked(bool checked)
{
    if (checked) {
        ui->value_numberOfCore->hide();
    }
}

void MLClassificationBoostingForm::on_manual_seed_clicked(bool checked)
{
    if (checked) {
        ui->value_seed->show();
    }
}

void MLClassificationBoostingForm::on_auto_seed_clicked(bool checked)
{
    if (checked) {
        ui->value_seed->hide();
    }
}

void MLClassificationBoostingForm::on_manual_TS_clicked(bool checked)
{
    if (checked) {
        ui->value_depthOfTree->show();
        ui->value_MinTermNodeSide->show();
        ui->label_16->show();
        ui->label_20->show();

        ui->value_depthOfTreeMin->hide();
        ui->value_depthOfTreeMax->hide();
        ui->value_depthOfTreeStep->hide();
        ui->value_MinTermNodeSideMin->hide();
        ui->value_MinTermNodeSideMax->hide();
        ui->value_MinTermNodeSideStep->hide();
        ui->label_8->hide();
        ui->label_12->hide();
        ui->label_7->hide();
        ui->label_10->hide();
        ui->label_11->hide();
    }
}

void MLClassificationBoostingForm::on_optimized_TS_clicked(bool checked)
{
    if (checked) {
        ui->value_depthOfTree->hide();
        ui->value_MinTermNodeSide->hide();
        ui->label_16->hide();
        ui->label_20->hide();

        ui->value_depthOfTreeMin->show();
        ui->value_depthOfTreeMax->show();
        ui->value_depthOfTreeStep->show();
        ui->value_MinTermNodeSideMin->show();
        ui->value_MinTermNodeSideMax->show();
        ui->value_MinTermNodeSideStep->show();
        ui->label_8->show();
        ui->label_12->show();
        ui->label_7->show();
        ui->label_10->show();
        ui->label_11->show();
    }
}

void MLClassificationBoostingForm::on_auto_TS_clicked(bool checked)
{
    if (checked) {
        ui->value_depthOfTree->hide();
        ui->value_MinTermNodeSide->hide();
        ui->label_16->hide();
        ui->label_20->hide();

        ui->value_depthOfTreeMin->hide();
        ui->value_depthOfTreeMax->hide();
        ui->value_depthOfTreeStep->hide();
        ui->value_MinTermNodeSideMin->hide();
        ui->value_MinTermNodeSideMax->hide();
        ui->value_MinTermNodeSideStep->hide();
        ui->label_8->hide();
        ui->label_12->hide();
        ui->label_7->hide();
        ui->label_10->hide();
        ui->label_11->hide();
    }
}

