//
// Copyright (C) 2018 University of Amsterdam
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

#include "multinomialtestform.h"
#include "ui_multinomialtestform.h"

MultinomialTestForm::MultinomialTestForm(QWidget *parent) :
	AnalysisForm("MultinomialTestForm", parent),
	ui(new Ui::MultinomialTestForm)
{
	ui->setupUi(this);

	_options = NULL;

	ui->listAvailableVariables->setModel(&_availableVariablesModel);
	ui->listAvailableVariables->setDoubleClickTarget(ui->factor);

	factorModel = new TableModelVariablesAssigned(this);
	factorModel->setSource(&_availableVariablesModel);
	factorModel->setVariableTypesSuggested(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	factorModel->setVariableTypesAllowed(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal | Column::ColumnTypeNominalText);

	ui->factor->setModel(factorModel);
	ui->factor->setDoubleClickTarget(ui->listAvailableVariables);
	ui->assignFactor->setSourceAndTarget(ui->listAvailableVariables, ui->factor);

	TableModelVariablesAssigned *countModel = new TableModelVariablesAssigned(this);
	countModel->setSource(&_availableVariablesModel);
	countModel->setVariableTypesSuggested(Column::ColumnTypeOrdinal | Column::ColumnTypeScale);
	countModel->setVariableTypesAllowed(Column::ColumnTypeNominal | Column::ColumnTypeScale);

	ui->counts->setModel(countModel);
	ui->counts->setDoubleClickTarget(ui->listAvailableVariables);
	ui->assignCounts->setSourceAndTarget(ui->listAvailableVariables, ui->counts);

	TableModelVariablesAssigned *probVarModel = new TableModelVariablesAssigned(this);
	probVarModel->setSource(&_availableVariablesModel);
	probVarModel->setVariableTypesSuggested(Column::ColumnTypeOrdinal | Column::ColumnTypeScale);
	probVarModel->setVariableTypesAllowed(Column::ColumnTypeNominal | Column::ColumnTypeScale);

	ui->exProbVar->setModel(probVarModel);
	ui->exProbVar->setDoubleClickTarget(ui->listAvailableVariables);
	ui->assignExProbVar->setSourceAndTarget(ui->listAvailableVariables, ui->exProbVar);

	ui->widget_expectedProbsTable->hide();

	connect(factorModel, SIGNAL(assignmentsChanged()), this, SLOT(addFixedFactors()));

	// TODO: Remove exProbVar if counts is empty
	// connect(countModel, SIGNAL(assignmentsChanged()), this, SLOT(countModelHandler()));

	connect(probVarModel, SIGNAL(assignmentsChanged()), this, SLOT(expectedCountsHandler()));
	connect(ui->tableWidget, SIGNAL(cellChanged(int, int)), this, SLOT(cellChangedHandler()));
}

MultinomialTestForm::~MultinomialTestForm()
{
	delete ui;
}

void MultinomialTestForm::bindTo(Options *options, DataSet *dataSet)
{
	AnalysisForm::bindTo(options, dataSet);

	_options = options;

	if (options != NULL && options->get("factor") != NULL) {
		_previous = dynamic_cast<OptionVariable *>(_options->get("factor"))->variable();

		if (_previous != "") {
			Labels labels = _dataSet->column(_previous).labels();
			verticalLabels = QStringList();

			for (LabelVector::const_iterator it = labels.begin(); it != labels.end(); ++it) {
				const Label &label = *it;
				verticalLabels << QString::fromStdString(label.text());
			}

			setTableVerticalHeaders();
			ui->tableWidget->updateTableValues(true);
		}
	}

	int columns = ui->tableWidget->columnCount();
	ui->deleteColumn->setEnabled(true);
	ui->addColumn->setEnabled(true);

	if (columns >= 5) {
		ui->addColumn->setEnabled(false);
	} else if (columns <= 0) {
		ui->deleteColumn->setEnabled(false);
	}

	// TODO: Remove exProbVar if counts is empty
	// countModelHandler();

	expectedCountsHandler();
}

void MultinomialTestForm::expectedCountsHandler()
{
	if (_options != NULL && _options->get("exProbVar") != NULL) {
		std::string column = dynamic_cast<OptionVariable *>(_options->get("exProbVar"))->variable();

		if (column == "") {
			ui->hypothesis->setEnabled(true);
		} else {
			ui->hypothesis->setEnabled(false);
		}
	}
}

void MultinomialTestForm::countModelHandler()
{
	if (_options != NULL && _options->get("counts") != NULL) {
		std::string column = dynamic_cast<OptionVariable *>(_options->get("counts"))->variable();

		if (column == "") {
			ui->exProbVar->setEnabled(false);
		} else {
			ui->exProbVar->setEnabled(true);
		}
	}
}

void MultinomialTestForm::setTableVerticalHeaders()
{
	ui->tableWidget->blockSignals(true);

	int row = 0;
	for (QStringList::iterator it = verticalLabels.begin(); it != verticalLabels.end(); ++it, ++row)
	{
		QString s = *it;
		QTableWidgetItem *headerItem = new QTableWidgetItem();
		headerItem->setToolTip(s);
		// Handle very long column names
		if (s.length() > 7) {
			s.truncate(4);
			s += "...";
		}

		headerItem->setText(s);
		ui->tableWidget->setVerticalHeaderItem(row, headerItem);
	}

	ui->tableWidget->blockSignals(false);
}

void MultinomialTestForm::cellChangedHandler()
{
	ui->tableWidget->updateTableValues();
}

void MultinomialTestForm::addFixedFactors() {

	if (factorModel->assigned().asString() == _previous) {
		return;
	}

	_previous = factorModel->assigned().asString();
	resetTable();
}

void MultinomialTestForm::resetTable() {
	ui->tableWidget->blockSignals(true);
	// Clear table contents before updating it with values
	ui->tableWidget->clearContents();
	ui->tableWidget->setRowCount(0);
	ui->tableWidget->setColumnCount(0);

	// Get the column from dataSet
	if (_dataSet != NULL && factorModel->assigned().size() > 0) {
		Labels labels = _dataSet->column(factorModel->assigned().asString()).labels();
		verticalLabels = QStringList();
		int rowCount = 0;

		// labels for the current column
		for (LabelVector::const_iterator it = labels.begin(); it != labels.end(); ++it, rowCount++) {
			const Label &label = *it;
			verticalLabels << QString::fromStdString(label.text());
		}

		ui->tableWidget->setRowCount(rowCount);
		// ui->tableWidget->setVerticalHeaderLabels(verticalLabels);
		setTableVerticalHeaders();
		addColumnToTable();
	}

	ui->tableWidget->blockSignals(false);
	ui->tableWidget->updateTableValues();

	int columns = ui->tableWidget->columnCount();
	if (columns <= 1) {
		ui->deleteColumn->setEnabled(false);
	}
	ui->addColumn->setEnabled(true);
}

void MultinomialTestForm::addColumnToTable() {
	ui->tableWidget->blockSignals(true);

	int columnCount = ui->tableWidget->columnCount() + 1;
	int rowCount = ui->tableWidget->rowCount();

	// Add column labels (Hypotheses labels)

	horizontalLabels = QStringList();
	QString letters[5] = {"a", "b", "c", "d", "e"};
	for (int col = 1; col <= columnCount; ++col) {
		horizontalLabels << "H₀ (" + letters[col-1] + ")";
	}
	ui->tableWidget->setColumnCount(columnCount);
	ui->tableWidget->setHorizontalHeaderLabels(horizontalLabels);

	// Initialize each cell in the new column with 1
	for (int row = 0; row < rowCount; ++row) {
		QTableWidgetItem *cellItem = new QTableWidgetItem(QString::number(1));
		ui->tableWidget->setItem(row, columnCount - 1, cellItem);
	}

	// If there are 5 hypotheses, then disable addColumn button
	if (columnCount >= 5) {
		ui->addColumn->setEnabled(false);
	}
	ui->deleteColumn->setEnabled(true);
	ui->tableWidget->blockSignals(false);
}

bool MultinomialTestForm::deleteColumnFromTable() {
	ui->tableWidget->blockSignals(true);

	int currentColumn = ui->tableWidget->currentColumn();
	if (currentColumn == -1) {
		return false;
	}
	ui->tableWidget->removeColumn(currentColumn);

	horizontalLabels = QStringList();
	int columns = ui->tableWidget->columnCount();

	// Assign the new hypothesis labels
	QString letters[5] = {"a", "b", "c", "d", "e"};
	for (int i = 0; i < columns; ++i) {
		horizontalLabels << QString::fromUtf8("H\u2080 (") + letters[i] + QString::fromUtf8(")");
	}

	ui->tableWidget->setHorizontalHeaderLabels(horizontalLabels);
	ui->tableWidget->blockSignals(false);

	ui->addColumn->setEnabled(true);
	if (columns <= 1) {
		ui->deleteColumn->setEnabled(false);
	}

	return true;
}

void MultinomialTestForm::on_addColumn_clicked(bool checked)
{
	addColumnToTable();
	ui->tableWidget->updateTableValues();
}

void MultinomialTestForm::on_deleteColumn_clicked(bool checked)
{
	if (deleteColumnFromTable()) {
		ui->tableWidget->updateTableValues();
	}
}

void MultinomialTestForm::on_resetColumns_clicked(bool checked)
{
	resetTable();
}
