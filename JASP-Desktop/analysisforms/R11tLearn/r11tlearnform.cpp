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

#include "r11tlearnform.h"
#include "ui_r11tlearnform.h"

#include "column.h"
#include "widgets/tablemodelvariablesassigned.h"
#include "widgets/tablemodelanovamodelnuisancefactors.h"

R11tLearnForm::R11tLearnForm(QWidget *parent) :
	AnalysisForm("R11tLearnForm", parent),
	ui(new Ui::R11tLearnForm)
{
	ui->setupUi(this);

	ui->listAvailableFields->setModel(&_availableVariablesModel);

	_subjectIdListModel = new TableModelVariablesAssigned(this);
	_subjectIdListModel->setVariableTypesSuggested(Column::ColumnTypeNominal);
	_subjectIdListModel->setSource(&_availableVariablesModel);
	ui->subjectId->setModel(_subjectIdListModel);

	_groupListModel = new TableModelVariablesAssigned(this);
	_groupListModel->setVariableTypesSuggested(Column::ColumnTypeNominal);
	_groupListModel->setVariableTypesAllowed(Column::ColumnTypeNominalText | Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	_groupListModel->setSource(&_availableVariablesModel);
	ui->group->setModel(_groupListModel);

	_trialNumberListModel = new TableModelVariablesAssigned(this);
	_trialNumberListModel->setSource(&_availableVariablesModel);
	_trialNumberListModel->setVariableTypesSuggested(Column::ColumnTypeOrdinal);
	ui->trialNumber->setModel(_trialNumberListModel);

	_deckListModel = new TableModelVariablesAssigned(this);
	_deckListModel->setVariableTypesSuggested(Column::ColumnTypeNominal);
	_deckListModel->setSource(&_availableVariablesModel);
	ui->deck->setModel(_deckListModel);

	_rewardListModel = new TableModelVariablesAssigned(this);
	_rewardListModel->setSource(&_availableVariablesModel);
	_rewardListModel->setVariableTypesSuggested(Column::ColumnTypeScale);
	_rewardListModel->setVariableTypesAllowed(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal | Column::ColumnTypeScale);
	ui->reward->setModel(_rewardListModel);

	_lossListModel = new TableModelVariablesAssigned(this);
	_lossListModel->setSource(&_availableVariablesModel);
	_lossListModel->setVariableTypesSuggested(Column::ColumnTypeScale);
	_lossListModel->setVariableTypesAllowed(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal | Column::ColumnTypeScale);
	ui->loss->setModel(_lossListModel);

	ui->buttonAssignSubjectId->setSourceAndTarget(ui->listAvailableFields, ui->subjectId);
	ui->buttonAssignGroup->setSourceAndTarget(ui->listAvailableFields, ui->group);
	ui->buttonAssignTrialNumber->setSourceAndTarget(ui->listAvailableFields, ui->trialNumber);
	ui->buttonAssignDeck->setSourceAndTarget(ui->listAvailableFields, ui->deck);
	ui->buttonAssignReward->setSourceAndTarget(ui->listAvailableFields, ui->reward);
	ui->buttonAssignLoss->setSourceAndTarget(ui->listAvailableFields, ui->loss);

	ui->containerParameters->hide();
	ui->containerSampling->hide();
}

R11tLearnForm::~R11tLearnForm()
{
	delete ui;
}
