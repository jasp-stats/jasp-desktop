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

#include "boundassignwidget.h"
#include "ui_boundassignwidget.h"

BoundAssignWidget::BoundAssignWidget(QWidget *parent) :
	QWidget(parent),
	ui(new Ui::BoundAssignWidget)
{
	ui->setupUi(this);

	ui->leftLabel->hide();
	ui->rightLabel->hide();

	_availableModel = new TableModelVariablesAvailable(this);
	_assignedModel = new TableModelVariablesAssigned(this);

	_assignedModel->setSource(_availableModel);
	_assignedModel->setSorted(true);

	ui->availableVariables->setModel(_availableModel);
	ui->availableVariables->setDoubleClickTarget(ui->assignedVariables);

	ui->assignedVariables->setModel(_assignedModel);
	ui->assignedVariables->setDoubleClickTarget(ui->availableVariables);

	ui->assignButton->setSourceAndTarget(ui->availableVariables, ui->assignedVariables);
}

BoundAssignWidget::~BoundAssignWidget()
{
	delete ui;
}

void BoundAssignWidget::bindTo(Option *option)
{
	ui->assignedVariables->bindTo(option);
}

void BoundAssignWidget::setVariables(const Terms &variables)
{
	_availableModel->setVariables(variables);
	_availableModel->notifyAlreadyAssigned(_assignedModel->assigned());
}

void BoundAssignWidget::setLabels(const QString &left, const QString &right)
{
	ui->leftLabel->setText(left);
	ui->rightLabel->setText(right);
	if (left.isEmpty()) ui->leftLabel->hide();
	else ui->leftLabel->show();
	if (right.isEmpty()) ui->rightLabel->hide();
	else ui->rightLabel->show();

}
