//
// Copyright (C) 2013-2018 University of Amsterdam
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
    variablesModel->setVariableTypesSuggested(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal | Column::ColumnTypeScale);
    variablesModel->setVariableTypesAllowed(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal | Column::ColumnTypeScale);
    ui->variables->setModel(variablesModel);
    ui->variables->setDoubleClickTarget(ui->listAvailableFields);

    ui->listAvailableFields_layout->setModel(&_availableVariablesModel);

    TableModelVariablesAssigned *groupingVariableModel = new TableModelVariablesAssigned(this);
    groupingVariableModel->setVariableTypesSuggested(Column::ColumnTypeOrdinal | Column::ColumnTypeNominal | Column::ColumnTypeNominalText);
    groupingVariableModel->setVariableTypesAllowed(Column::ColumnTypeOrdinal | Column::ColumnTypeNominal | Column::ColumnTypeNominalText);
    groupingVariableModel->setSource(&_availableVariablesModel);
    ui->groupingVariable->setModel(groupingVariableModel);
    ui->groupingVariable->setDoubleClickTarget(ui->listAvailableFields_layout);

    ui->listAvailableFields_nodes->setModel(&_availableVariablesModel);

    TableModelVariablesAssigned *colorNodesByModel = new TableModelVariablesAssigned(this);
    colorNodesByModel->setVariableTypesSuggested(Column::ColumnTypeNominalText);
    colorNodesByModel->setVariableTypesAllowed(Column::ColumnTypeNominalText);
    colorNodesByModel->setSource(&_availableVariablesModel);
    ui->colorNodesBy->setModel(colorNodesByModel);
    ui->colorNodesBy->setDoubleClickTarget(ui->listAvailableFields_nodes);

    TableModelVariablesAssigned *mgmVariableTypeModel = new TableModelVariablesAssigned(this);
    mgmVariableTypeModel->setVariableTypesSuggested(Column::ColumnTypeNominalText);
    mgmVariableTypeModel->setVariableTypesAllowed(Column::ColumnTypeNominalText);
    mgmVariableTypeModel->setSource(&_availableVariablesModel);
    ui->mgmVariableType->setModel(mgmVariableTypeModel);
    ui->mgmVariableType->setDoubleClickTarget(ui->listAvailableFields);

    // layout
    TableModelVariablesAssigned *layoutXModel = new TableModelVariablesAssigned(this);
    layoutXModel->setVariableTypesSuggested(Column::ColumnTypeNominalText);
    layoutXModel->setVariableTypesAllowed(Column::ColumnTypeNominalText);
    layoutXModel->setSource(&_availableVariablesModel);
    ui->layoutX->setModel(layoutXModel);
    ui->layoutX->setDoubleClickTarget(ui->listAvailableFields_layout);

    TableModelVariablesAssigned *layoutYModel = new TableModelVariablesAssigned(this);
    layoutYModel->setVariableTypesSuggested(Column::ColumnTypeNominalText);
    layoutYModel->setVariableTypesAllowed(Column::ColumnTypeNominalText);
    layoutYModel->setSource(&_availableVariablesModel);
    ui->layoutY->setModel(layoutYModel);
    ui->layoutY->setDoubleClickTarget(ui->listAvailableFields_layout);

    ui->buttonAssignVariables->setSourceAndTarget(ui->listAvailableFields, ui->variables);
    ui->buttonAssignGroupingVariable->setSourceAndTarget(ui->listAvailableFields, ui->groupingVariable);
    ui->buttonAssignColorNodesBy->setSourceAndTarget(ui->listAvailableFields_nodes, ui->colorNodesBy);
    ui->buttonAssignMgmVariableType->setSourceAndTarget(ui->listAvailableFields, ui->mgmVariableType);
    ui->buttonAssignLayoutX->setSourceAndTarget(ui->listAvailableFields_layout, ui->layoutX);
    ui->buttonAssignLayoutY->setSourceAndTarget(ui->listAvailableFields_layout, ui->layoutY);

    // Defaults
    ui->estimator->setCurrentIndex(0);
    ui->analysisOptions->hide();
    ui->bootstrapOptions->hide();
    ui->graphicalOptions->hide();
    ui->layout->hide();
    ui->layoutDataBox->hide();
    ui->nodes->hide();

    ui->_1spring->setChecked(true);
    ui->label_repulsion->setVisible(true);
    ui->repulsion->setVisible(true);
    ui->_4parametric->hide();
    ui->_5person->hide();
    ui->_6jackknife->hide();
    ui->showMgmVariableType->setVisible(false);
    
    // for the next release
#ifndef JASP_DEBUG
//    ui->plotClustering->setVisible(false);
//    ui->tableClustering->setVisible(false);
    ui->parallelBootstrap->setVisible(false);
#endif
    
}

NetworkAnalysisForm::~NetworkAnalysisForm()
{
    delete ui;
}

void NetworkAnalysisForm::on_estimator_currentIndexChanged(const QString &choice)
{
    std::string choice_str = choice.toStdString();

    ui->gridLayout_3->removeWidget(ui->correlationMethod);
    ui->gridLayout_3->removeWidget(ui->tuningParameterBox);
    ui->gridLayout_3->removeWidget(ui->sampleSize);
    ui->gridLayout_3->removeWidget(ui->missingValues);
    ui->gridLayout_3->removeWidget(ui->criterion);
    ui->gridLayout_3->removeWidget(ui->isingEstimator);
    ui->gridLayout_3->removeWidget(ui->crossValidation);
    ui->gridLayout_3->removeWidget(ui->split);
    ui->gridLayout_3->removeWidget(ui->rule);
    ui->gridLayout_3->removeWidget(ui->boxMgmVariableType);
    ui->gridLayout_3->removeWidget(ui->thresholdBox);
    ui->gridLayout_3->removeWidget(ui->network);
    ui->gridLayout_3->removeWidget(ui->normalizeCentrality);

    ui->correlationMethod->hide();
    ui->tuningParameterBox->hide();
    ui->sampleSize->hide();
    ui->missingValues->hide();
    ui->criterion->hide();
    ui->isingEstimator->hide();
    ui->crossValidation->hide();
    ui->split->hide();
    ui->rule->hide();
    ui->boxMgmVariableType->hide();
    ui->thresholdBox->hide();
    ui->network->hide();
    ui->normalizeCentrality->hide();
    ui->showMgmVariableType->setVisible(false);

    if (choice_str.compare("EBICglasso") == 0) {
        ui->correlationMethod->setVisible(true);
        ui->tuningParameterBox->setVisible(true);
        ui->sampleSize->setVisible(true);
        ui->missingValues->setVisible(true);
        ui->normalizeCentrality->setVisible(true);
        ui->network->setVisible(true);

        ui->analysisOptionsExpander->setText("Analysis Options - EBICglasso");
        ui->gridLayout_3->addWidget(ui->correlationMethod, 0, 0);
        ui->gridLayout_3->addWidget(ui->normalizeCentrality, 0, 1);
        ui->gridLayout_3->addWidget(ui->network, 1, 0);
        ui->gridLayout_3->addWidget(ui->missingValues, 1, 1);
        ui->gridLayout_3->addWidget(ui->sampleSize, 2, 0);
        ui->gridLayout_3->addWidget(ui->tuningParameterBox, 2, 1);

    } else if (choice_str.compare("cor") == 0) {
        ui->correlationMethod->setVisible(true);
        ui->missingValues->setVisible(true);
        ui->thresholdBox->setVisible(true);

        ui->analysisOptionsExpander->setText("Analysis Options - cor");
        ui->gridLayout_3->addWidget(ui->correlationMethod, 0, 0);
        ui->gridLayout_3->addWidget(ui->missingValues, 0, 1);
        ui->gridLayout_3->addWidget(ui->thresholdBox, 1, 0);

    } else if (choice_str.compare("pcor") == 0) {
        ui->correlationMethod->setVisible(true);
        ui->missingValues->setVisible(true);
        ui->thresholdBox->setVisible(true);

        ui->analysisOptionsExpander->setText("Analysis Options - pcor");
        ui->gridLayout_3->addWidget(ui->correlationMethod, 0, 0);
        ui->gridLayout_3->addWidget(ui->missingValues, 0, 1);
        ui->gridLayout_3->addWidget(ui->thresholdBox, 1, 0);

    } else if (choice_str.compare("IsingFit") == 0) {
        ui->tuningParameterBox->setVisible(true);
        ui->split->setVisible(true);
        ui->rule->setVisible(true);

        ui->analysisOptionsExpander->setText("Analysis Options - IsingFit");
        ui->gridLayout_3->addWidget(ui->rule, 0, 0);
        ui->gridLayout_3->addWidget(ui->split, 0, 1);
        ui->gridLayout_3->addWidget(ui->tuningParameterBox, 1, 0);

    } else if (choice_str.compare("IsingSampler") == 0) {
        ui->isingEstimator->setVisible(true);
        ui->split->setVisible(true);

        ui->analysisOptionsExpander->setText("Analysis Options - IsingSampler");
        ui->gridLayout_3->addWidget(ui->isingEstimator, 0, 0);
        ui->gridLayout_3->addWidget(ui->split, 0, 1);

    } else if (choice_str.compare("huge") == 0) {
        ui->tuningParameterBox->setVisible(true);
        ui->criterion->setVisible(true);
        ui->_4cv->setEnabled(false);

        ui->analysisOptionsExpander->setText("Analysis Options - huge");
        ui->gridLayout_3->addWidget(ui->criterion, 0, 0);
        ui->gridLayout_3->addWidget(ui->tuningParameterBox, 0, 1);

    } else if (choice_str.compare("adalasso") == 0) {
        ui->crossValidation->setVisible(true);

        ui->analysisOptionsExpander->setText("Analysis Options - adalasso");
        ui->gridLayout_3->addWidget(ui->crossValidation, 0, 0);

    } else if (choice_str.compare("mgm") == 0) {
        ui->tuningParameterBox->setVisible(true);
        ui->criterion->setVisible(true);
        ui->rule->setVisible(true);
        ui->boxMgmVariableType->setVisible(true);
        ui->crossValidation->setVisible(false);
        ui->showMgmVariableType->setVisible(true);
        
//        ui->_4cv->setEnabled(true);

        ui->analysisOptionsExpander->setText("Analysis Options - mgm");
        ui->gridLayout_3->addWidget(ui->tuningParameterBox, 1, 0);
        ui->gridLayout_3->addWidget(ui->criterion, 0, 0);
        ui->gridLayout_3->addWidget(ui->rule, 0, 1);
        ui->gridLayout_3->addWidget(ui->boxMgmVariableType, 2, 0);
        ui->gridLayout_3->addWidget(ui->crossValidation, 1, 1);
        ui->showMgmVariableType->setVisible(true);
        
        if(ui->_4cv->isChecked()) {
            ui->crossValidation->setVisible(true);        
        }
    }

    ui->analysisOptions->setLayout(ui->gridLayout_3);

}

void NetworkAnalysisForm::on__4cv_clicked()
{
    if (ui->estimator->currentText() == "mgm") {
        ui->crossValidation->setVisible(true);
    } else {
        ui->crossValidation->setVisible(false);
    }
}

void NetworkAnalysisForm::on__3stars_clicked()
{
    if (ui->estimator->currentText() == "mgm") {
        ui->crossValidation->setVisible(false);
    }
}

void NetworkAnalysisForm::on__2ric_clicked()
{
    if (ui->estimator->currentText() == "mgm") {
        ui->crossValidation->setVisible(false);
    }
}

void NetworkAnalysisForm::on__1ebic_clicked()
{
    if (ui->estimator->currentText() == "mgm") {
        ui->crossValidation->setVisible(false);
    }
}

void NetworkAnalysisForm::on_graphicalOptionsExpander_clicked()
{
    if (! ui->_3Data->isChecked()) {
        ui->layoutDataBox->setVisible(false);
    }
}

void NetworkAnalysisForm::on__1spring_clicked(bool checked)
{
    if (checked) {
        ui->layoutDataBox->setVisible(false);
        ui->label_repulsion->setVisible(true);
        ui->repulsion->setVisible(true);
    }
}

void NetworkAnalysisForm::on__2circle_clicked(bool checked)
{
    if (checked) {
        ui->layoutDataBox->setVisible(false);
        ui->label_repulsion->setVisible(false);
        ui->repulsion->setVisible(false);
    }
}

void NetworkAnalysisForm::on__3Data_clicked(bool checked)
{
    if (checked) {
        ui->layoutDataBox->setVisible(true);
        ui->label_repulsion->setVisible(false);
        ui->repulsion->setVisible(false);
    }
}
