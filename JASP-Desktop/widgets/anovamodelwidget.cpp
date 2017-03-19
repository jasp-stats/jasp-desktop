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

#include "anovamodelwidget.h"
#include "ui_anovamodelwidget.h"

#include <boost/foreach.hpp>
#include <QMenu>
#include <QDebug>

#include "analysisforms/analysisform.h"
#include "draganddrop.h"

using namespace std;

AnovaModelWidget::AnovaModelWidget(QWidget *parent) :
	QWidget(parent),
	ui(new Ui::AnovaModelWidget)
{
	ui->setupUi(this);

	_boundTo = NULL;
	_tableModelAnovaModel = NULL;

	_tableModelVariablesAvailable = new TableModelVariablesAvailable(this);
	_tableModelVariablesAvailable->setSupportedDragActions(Qt::CopyAction);
	_tableModelVariablesAvailable->setSupportedDropActions(Qt::MoveAction);
	_tableModelVariablesAvailable->setMimeType("application/vnd.list.term");
	ui->listFactorsAvailable->setModel(_tableModelVariablesAvailable);
	ui->listFactorsAvailable->setDoubleClickTarget(ui->listModelTerms);
	ui->listFactorsAvailable->selectionUpdated.connect(boost::bind(&AnovaModelWidget::sourceSelectionChanged, this));

	ui->listModelTerms->setDoubleClickTarget(ui->listFactorsAvailable);
	ui->listModelTerms->horizontalHeader()->setStretchLastSection(true);

	ui->buttonAssignCross->setSourceAndTarget(ui->listFactorsAvailable, ui->listModelTerms);
	ui->buttonAssignMenu->setSourceAndTarget(ui->listFactorsAvailable, ui->listModelTerms);

	_assignInteraction = new QAction("Interaction", this);
	_assignMainEffects = new QAction("Main Effects", this);
	_assign2ways = new QAction("All 2 way", this);
	_assign3ways = new QAction("All 3 way", this);
	_assign4ways = new QAction("All 4 way", this);
	_assign5ways = new QAction("All 5 way", this);

	connect(_assignInteraction, SIGNAL(triggered()), this, SLOT(assignInteraction()));
	connect(_assignMainEffects, SIGNAL(triggered()), this, SLOT(assignMainEffects()));
	connect(_assign2ways, SIGNAL(triggered()), this, SLOT(assign2ways()));
	connect(_assign3ways, SIGNAL(triggered()), this, SLOT(assign3ways()));
	connect(_assign4ways, SIGNAL(triggered()), this, SLOT(assign4ways()));
	connect(_assign5ways, SIGNAL(triggered()), this, SLOT(assign5ways()));

	QList<QAction *> actions;
	actions.append(_assignInteraction);
	actions.append(_assignMainEffects);
	actions.append(_assign2ways);
	actions.append(_assign3ways);
	actions.append(_assign4ways);
	actions.append(_assign5ways);

	QMenu *menu = new QMenu(ui->buttonAssignMenu);
	menu->addActions(actions);
	ui->buttonAssignMenu->setMenu(menu);

	ui->listModelTerms->horizontalHeader()->setDefaultSectionSize(230);
	ui->listModelTerms->horizontalHeader()->setStretchLastSection(true);
}

AnovaModelWidget::~AnovaModelWidget()
{
	delete ui;
}

void AnovaModelWidget::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionsTable *>(option);

	if (_tableModelAnovaModel != NULL && _boundTo != NULL)
	{
		_tableModelAnovaModel->bindTo(_boundTo);

		ui->columnLabel0->setText(_tableModelAnovaModel->headerData(0, Qt::Horizontal, Qt::DisplayRole).toString());
		if (_tableModelAnovaModel->columnCount() > 1)
			ui->columnLabel1->setText(_tableModelAnovaModel->headerData(1, Qt::Horizontal, Qt::DisplayRole).toString());
	}
}

void AnovaModelWidget::setModel(TableModelAnovaModel *model)
{
	_tableModelAnovaModel = model;

	if (_boundTo != NULL)
		_tableModelAnovaModel->bindTo(_boundTo);

	ui->buttonAssignMenu->setVisible(_tableModelAnovaModel->piecesCanBeAssigned());
	ui->listModelTerms->setModel(model);

	variablesAvailableChanged();
	connect(_tableModelAnovaModel, SIGNAL(variablesAvailableChanged()), this, SLOT(variablesAvailableChanged()));
}

void AnovaModelWidget::setFactorsLabel(const QString &label)
{
	ui->factorsLabel->setText(label);
}

void AnovaModelWidget::variablesAvailableChanged()
{
	if (_tableModelAnovaModel == NULL)
		return;

	const Terms &variables = _tableModelAnovaModel->variables();
	_tableModelVariablesAvailable->setVariables(variables);
}

void AnovaModelWidget::sourceSelectionChanged()
{
	int selectedCount = ui->listFactorsAvailable->selectionModel()->selectedIndexes().size();

	_assignInteraction->setEnabled(selectedCount >= 2);
	_assignMainEffects->setEnabled(selectedCount >= 1);
	_assign2ways->setEnabled(selectedCount >= 2);
	_assign3ways->setEnabled(selectedCount >= 3);
	_assign4ways->setEnabled(selectedCount >= 4);
	_assign5ways->setEnabled(selectedCount >= 5);
}

void AnovaModelWidget::assignInteraction()
{
	DragAndDrop::perform(ui->listFactorsAvailable, ui->listModelTerms, TableModelAnovaModel::Interaction);
}

void AnovaModelWidget::assignMainEffects()
{
	DragAndDrop::perform(ui->listFactorsAvailable, ui->listModelTerms, TableModelAnovaModel::MainEffects);
}

void AnovaModelWidget::assign2ways()
{
	DragAndDrop::perform(ui->listFactorsAvailable, ui->listModelTerms, TableModelAnovaModel::All2Way);
}

void AnovaModelWidget::assign3ways()
{
	DragAndDrop::perform(ui->listFactorsAvailable, ui->listModelTerms, TableModelAnovaModel::All3Way);
}

void AnovaModelWidget::assign4ways()
{
	DragAndDrop::perform(ui->listFactorsAvailable, ui->listModelTerms, TableModelAnovaModel::All4Way);
}

void AnovaModelWidget::assign5ways()
{
	DragAndDrop::perform(ui->listFactorsAvailable, ui->listModelTerms, TableModelAnovaModel::All5Way);
}
