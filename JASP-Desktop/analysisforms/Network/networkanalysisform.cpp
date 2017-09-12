//
// Copyright (C) 2013-2017 University of Amsterdam
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

#include "networkanalysisform.h"
#include "ui_networkanalysisform.h"


NetworkAnalysisForm::NetworkAnalysisForm(QWidget *parent) :
    AnalysisForm("NetworkAnalysisForm", parent),
    ui(new Ui::NetworkAnalysisForm)
{
    ui->setupUi(this);

    ui->listAvailableFields->setModel(&_availableVariablesModel);
    ui->listAvailableFields->setDoubleClickTarget(ui->variables);

    TableModelVariablesAssigned *variablesModel = new TableModelVariablesAssigned(this);
    variablesModel->setSource(&_availableVariablesModel);
    variablesModel->setVariableTypesSuggested(Column::ColumnTypeScale);
    variablesModel->setVariableTypesAllowed(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal | Column::ColumnTypeScale);
    ui->variables->setModel(variablesModel);
    ui->variables->setDoubleClickTarget(ui->listAvailableFields);

    TableModelVariablesAssigned *groupingVariableModel = new TableModelVariablesAssigned(this);
    groupingVariableModel->setVariableTypesAllowed(Column::ColumnTypeOrdinal | Column::ColumnTypeNominal);
    groupingVariableModel->setSource(&_availableVariablesModel);
    ui->groupingVariable->setModel(groupingVariableModel);
    ui->groupingVariable->setDoubleClickTarget(ui->listAvailableFields);

    TableModelVariablesAssigned *colorNodesByModel = new TableModelVariablesAssigned(this);
    colorNodesByModel->setVariableTypesAllowed(Column::ColumnTypeOrdinal | Column::ColumnTypeNominal);
    colorNodesByModel->setSource(&_availableVariablesModel);
    ui->colorNodesBy->setModel(colorNodesByModel);
    ui->colorNodesBy->setDoubleClickTarget(ui->listAvailableFields);

    TableModelVariablesAssigned *mgmVariableTypeModel = new TableModelVariablesAssigned(this);
    mgmVariableTypeModel->setVariableTypesAllowed(Column::ColumnTypeNominalText);
    mgmVariableTypeModel->setSource(&_availableVariablesModel);
    ui->mgmVariableType->setModel(mgmVariableTypeModel);
    ui->mgmVariableType->setDoubleClickTarget(ui->listAvailableFields);

    ui->buttonAssignVariables->setSourceAndTarget(ui->listAvailableFields, ui->variables);
    ui->buttonAssignGroupingVariable->setSourceAndTarget(ui->listAvailableFields, ui->groupingVariable);
    ui->buttonAssignColorNodesBy->setSourceAndTarget(ui->listAvailableFields, ui->colorNodesBy);
    ui->buttonAssignMgmVariableType->setSourceAndTarget(ui->listAvailableFields, ui->mgmVariableType);

    // Defaults
    ui->estimator->setCurrentIndex(0);
    ui->analysisOptions->hide();
    ui->bootstrapOptions->hide();
    ui->graphicalOptions->hide();
}

NetworkAnalysisForm::~NetworkAnalysisForm()
{
    delete ui;
}

void NetworkAnalysisForm::on_estimator_currentIndexChanged(const QString &choice)
{
    std::string choice_str = choice.toStdString();

    if (choice_str.compare("EBICglasso") == 0) {
        ui->correlationMethod->setEnabled(true);
        ui->tuningParameterBox->setEnabled(true);
        ui->sampleSize->setEnabled(true);
        ui->missingValues->setEnabled(true);

        ui->criterion->setEnabled(false);
        ui->_4cv->setEnabled(false);
        ui->isingEstimator->setEnabled(false);
        ui->crossValidation->setEnabled(false);
        ui->split->setEnabled(false);
        ui->rule->setEnabled(false);
        ui->boxMgmVariableType->setEnabled(false);
        ui->showMgmVariableType->setEnabled(false);

    } else if (choice_str.compare("cor") == 0) {
        ui->correlationMethod->setEnabled(true);
        ui->missingValues->setEnabled(true);

        ui->criterion->setEnabled(false);
        ui->tuningParameterBox->setEnabled(false);
        ui->sampleSize->setEnabled(false);
        ui->isingEstimator->setEnabled(false);
        ui->crossValidation->setEnabled(false);
        ui->split->setEnabled(false);
        ui->rule->setEnabled(false);
        ui->boxMgmVariableType->setEnabled(false);
        ui->showMgmVariableType->setEnabled(false);

    } else if (choice_str.compare("pcor") == 0) {
        ui->correlationMethod->setEnabled(true);
        ui->missingValues->setEnabled(true);

        ui->criterion->setEnabled(false);
        ui->tuningParameterBox->setEnabled(false);
        ui->sampleSize->setEnabled(false);
        ui->isingEstimator->setEnabled(false);
        ui->crossValidation->setEnabled(false);
        ui->split->setEnabled(false);
        ui->rule->setEnabled(false);
        ui->boxMgmVariableType->setEnabled(false);
        ui->showMgmVariableType->setEnabled(false);

    } else if (choice_str.compare("IsingFit") == 0) {
        ui->tuningParameterBox->setEnabled(true);
        ui->split->setEnabled(true);
        ui->rule->setEnabled(true);

        ui->correlationMethod->setEnabled(false);
        ui->criterion->setEnabled(false);
        ui->sampleSize->setEnabled(false);
        ui->isingEstimator->setEnabled(false);
        ui->crossValidation->setEnabled(false);
        ui->missingValues->setEnabled(false);
        ui->boxMgmVariableType->setEnabled(false);
        ui->showMgmVariableType->setEnabled(false);

    } else if (choice_str.compare("IsingSampler") == 0) {
        ui->isingEstimator->setEnabled(true);
        ui->split->setEnabled(true);

        ui->tuningParameterBox->setEnabled(false);
        ui->rule->setEnabled(false);
        ui->correlationMethod->setEnabled(false);
        ui->criterion->setEnabled(false);
        ui->sampleSize->setEnabled(false);
        ui->crossValidation->setEnabled(false);
        ui->missingValues->setEnabled(false);
        ui->boxMgmVariableType->setEnabled(false);
        ui->showMgmVariableType->setEnabled(false);

    } else if (choice_str.compare("huge") == 0) {
        ui->tuningParameterBox->setEnabled(true);
        ui->criterion->setEnabled(true);

        ui->_4cv->setEnabled(false);
        ui->isingEstimator->setEnabled(false);
        ui->split->setEnabled(false);
        ui->rule->setEnabled(false);
        ui->correlationMethod->setEnabled(false);
        ui->sampleSize->setEnabled(false);
        ui->crossValidation->setEnabled(false);
        ui->missingValues->setEnabled(false);
        ui->boxMgmVariableType->setEnabled(false);
        ui->showMgmVariableType->setEnabled(false);

    } else if (choice_str.compare("adalasso") == 0) {
        ui->crossValidation->setEnabled(true);

        ui->tuningParameterBox->setEnabled(false);
        ui->criterion->setEnabled(false);
        ui->isingEstimator->setEnabled(false);
        ui->split->setEnabled(false);
        ui->rule->setEnabled(false);
        ui->correlationMethod->setEnabled(false);
        ui->missingValues->setEnabled(false);
        ui->sampleSize->setEnabled(false);
        ui->boxMgmVariableType->setEnabled(false);
        ui->showMgmVariableType->setEnabled(false);

    } else if (choice_str.compare("mgm") == 0) {
        ui->tuningParameterBox->setEnabled(true);
        ui->criterion->setEnabled(true);
        ui->rule->setEnabled(true);
        ui->_4cv->setEnabled(true);
        ui->boxMgmVariableType->setEnabled(true);
        ui->showMgmVariableType->setEnabled(true);

        ui->isingEstimator->setEnabled(false);
        ui->split->setEnabled(false);
        ui->correlationMethod->setEnabled(false);
        ui->sampleSize->setEnabled(false);
        ui->missingValues->setEnabled(false);

        if (ui->_4cv->isChecked()) {
            ui->crossValidation->setEnabled(true);
        } else {
            ui->crossValidation->setEnabled(false);
        }
    }
}

void NetworkAnalysisForm::on__4cv_clicked()
{
    if (ui->estimator->currentIndex() == 6) {
        ui->crossValidation->setEnabled(true);
    } else {
        ui->crossValidation->setEnabled(false);
    }
}

void NetworkAnalysisForm::on__3stars_clicked()
{
    if (ui->estimator->currentIndex() == 6) {
        ui->crossValidation->setEnabled(false);
    }
}

void NetworkAnalysisForm::on__2ric_clicked()
{
    if (ui->estimator->currentIndex() == 6) {
        ui->crossValidation->setEnabled(false);
    }
}

void NetworkAnalysisForm::on__1ebic_clicked()
{
    if (ui->estimator->currentIndex() == 6) {
        ui->crossValidation->setEnabled(false);
    }
}
