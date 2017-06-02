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

#include "mlclusteringkmeansform.h"
#include "ui_mlclusteringkmeansform.h"

MLClusteringKMeansForm::MLClusteringKMeansForm(QWidget *parent) :
	AnalysisForm("MLClusteringKMeansForm", parent),
	ui(new Ui::MLClusteringKMeansForm)
{
	ui->setupUi(this);

	ui->listAvailableFields->setModel(&_availableVariablesModel);

	_predictorsListModel = new TableModelVariablesAssigned(this);
	_predictorsListModel->setSource(&_availableVariablesModel);
	_predictorsListModel->setVariableTypesSuggested(Column::ColumnTypeScale | Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->predictors->setModel(_predictorsListModel);

	ui->buttonAssignRandom->setSourceAndTarget(ui->listAvailableFields, ui->predictors);

	ui->advancedOptions->hide();

	defaultOptions();
}

MLClusteringKMeansForm::~MLClusteringKMeansForm()
{
	delete ui;
}

void MLClusteringKMeansForm::defaultOptions()
{
	QSizePolicy retain = ui->clusterSize->sizePolicy();
	retain.setRetainSizeWhenHidden(true);

	ui->clusterSize->setSizePolicy(retain);
	ui->clusterSize->hide();

	ui->optimizedFrom->setSizePolicy(retain);
	ui->optimizedFrom->hide();

	ui->optimizedTo->setSizePolicy(retain);
	ui->optimizedTo->hide();

	ui->robustFrom->setSizePolicy(retain);
	ui->robustFrom->hide();

	ui->robustTo->setSizePolicy(retain);
	ui->robustTo->hide();

	ui->iterationsCount->setSizePolicy(retain);
	ui->iterationsCount->hide();

	ui->randomSetCount->setSizePolicy(retain);
	ui->randomSetCount->hide();

	retain = ui->label_to_1->sizePolicy();
	retain.setRetainSizeWhenHidden(true);
	ui->label_to_1->setSizePolicy(retain);
	ui->label_to_1->hide();

	ui->label_to_2->setSizePolicy(retain);
	ui->label_to_2->hide();
}

void MLClusteringKMeansForm::bindTo(Options *options, DataSet *dataSet)
{
	AnalysisForm::bindTo(options, dataSet);

	factorsChanging();

	factorsChanged();
}

void MLClusteringKMeansForm::factorsChanging()
{
	if (_options != NULL)
		_options->blockSignals(true);
}

void MLClusteringKMeansForm::factorsChanged()
{
	if (_options != NULL)
		_options->blockSignals(false);
}

void MLClusteringKMeansForm::on_manual_1_clicked(bool checked)
{
	if (checked) {
		ui->clusterSize->show();
		ui->optimizedFrom->hide();
		ui->optimizedTo->hide();
		ui->robustFrom->hide();
		ui->robustTo->hide();
		ui->label_to_1->hide();
		ui->label_to_2->hide();
	}
}

void MLClusteringKMeansForm::on_auto_1_clicked(bool checked)
{
	if (checked) {
		ui->clusterSize->hide();
		ui->optimizedFrom->hide();
		ui->optimizedTo->hide();
		ui->robustFrom->hide();
		ui->robustTo->hide();
		ui->label_to_1->hide();
		ui->label_to_2->hide();
	}
}

void MLClusteringKMeansForm::on_optimized_1_clicked(bool checked)
{
	if (checked) {
		ui->clusterSize->hide();
		ui->optimizedFrom->show();
		ui->optimizedTo->show();
		ui->robustFrom->hide();
		ui->robustTo->hide();
		ui->label_to_1->show();
		ui->label_to_2->hide();
	}
}

void MLClusteringKMeansForm::on_robust_1_clicked(bool checked)
{
	if (checked) {
		ui->clusterSize->hide();
		ui->optimizedFrom->hide();
		ui->optimizedTo->hide();
		ui->robustFrom->show();
		ui->robustTo->show();
		ui->label_to_1->hide();
		ui->label_to_2->show();
	}
}

void MLClusteringKMeansForm::on_manual_2_clicked(bool checked)
{
	if (checked) {
		ui->iterationsCount->show();
	}
}

void MLClusteringKMeansForm::on_auto_2_clicked(bool checked)
{
	if (checked) {
		ui->iterationsCount->hide();
	}
}

void MLClusteringKMeansForm::on_manual_3_clicked(bool checked)
{
	if (checked) {
		ui->randomSetCount->show();
	}
}

void MLClusteringKMeansForm::on_auto_3_clicked(bool checked)
{
	if (checked) {
		ui->randomSetCount->hide();
	}
}
