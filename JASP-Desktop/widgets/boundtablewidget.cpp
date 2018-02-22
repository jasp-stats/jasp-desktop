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

#include "boundtablewidget.h"

BoundTableWidget::BoundTableWidget(QWidget *parent)
	: QTableWidget(parent)
{
	_boundTo = NULL;
}

void BoundTableWidget::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionsTable *>(option);

	if (_boundTo != NULL) {
		_groups = _boundTo->value();

		// Populate table from Options
		populateTableFromOption();
		
	}
}

void BoundTableWidget::populateTableFromOption()
{
	// For each QTableWidgetItem updation, a 'cellChanged' signal is triggered.
	// Block all such signals until Table updation
	this->blockSignals(true);
	_boundTo->blockSignals(true);

	// Initialize
	this->clearContents();
	this->setRowCount(0);
	this->setColumnCount(0);

	std::vector<std::vector<std::string> > verticalHeaders;
	std::vector<double> values;

	QStringList horizontalLabels = QStringList();
	QStringList verticalLabels = QStringList();
	int column = 0;
	bool setRowCount = false;

	for (std::vector<Options *>::iterator it = _groups.begin(); it != _groups.end(); ++it) {

		Options *newRow = static_cast<Options *>(*it);
		OptionString *factorName = static_cast<OptionString *>(newRow->get("name"));
		OptionVariables *headers = static_cast<OptionVariables *>(newRow->get("levels"));
		OptionDoubleArray *option = static_cast<OptionDoubleArray *>(newRow->get("values"));

		horizontalLabels << QString::fromStdString(factorName->value());
		values = option->value();
		verticalHeaders = headers->value();

		if (!setRowCount) {
			this->setRowCount(values.size());
			setRowCount = true;
		}

		this->insertColumn(column);

		int row = 0;
		for (std::vector<double>::iterator i = values.begin(); i != values.end(); ++i, ++row) {
			QTableWidgetItem *newItem = new QTableWidgetItem(QString::number(*i));
			this->setItem(row, column, newItem);
		}

		column++;
	}

	for (std::vector<std::vector<std::string> >::iterator it = verticalHeaders.begin(); it != verticalHeaders.end(); it++) {
		for(std::vector<std::string>::iterator col = it->begin(); col != it->end(); col++) {
			verticalLabels << QString::fromStdString(*col);
		}
	}

	this->setHorizontalHeaderLabels(horizontalLabels);
	this->setVerticalHeaderLabels(verticalLabels);

	updateTableValues();
	
	_boundTo->blockSignals(false, false);
	this->blockSignals(false);
}

void BoundTableWidget::updateTableValues(bool noSignal)
{
	if (_boundTo == NULL) {
		return;
	}

	int columnCount = this->columnCount();
	int rowCount = this->rowCount();

	_groups.clear();
	std::vector<std::string> verticalHeaders;

	// Get verticalHeaders
	for (int i = 0; i < rowCount; ++i) {
		QTableWidgetItem *verticalHeaderItem = this->verticalHeaderItem(i);
		QString header = "";
		if (verticalHeaderItem != NULL) {
			header = verticalHeaderItem->toolTip();  // Tooltip will have the entire text
		}
		verticalHeaders.push_back(fq(header));
	}

	for (int i = 0; i < columnCount; ++i) {
		Options *newRow = static_cast<Options *>(_boundTo->rowTemplate()->clone());
		OptionString *factorName = static_cast<OptionString *>(newRow->get("name"));
		QString header = "";
		QTableWidgetItem *horizontalHeaderItem = this->horizontalHeaderItem(i);

		if (horizontalHeaderItem != NULL) {
			header = horizontalHeaderItem->text();
		} else {
			// TODO: Revisit this part
			continue;
		}
		factorName->setValue(fq(header));

		OptionDoubleArray *option = static_cast<OptionDoubleArray *>(newRow->get("values"));
		OptionVariables *headers = static_cast<OptionVariables *>(newRow->get("levels"));
		headers->setValue(verticalHeaders);
		std::vector<double> values;
		std::vector<std::string> levels;

		for (int j = 0; j < rowCount; ++j) {
			QTableWidgetItem *rowItem = this->item(j, i);
			QString content = "";

			if (rowItem != NULL) {
				content = rowItem->text();
			}
			values.push_back(content.toDouble());
		}

		option->setValue(values);
		_groups.push_back(newRow);
	}

	if (noSignal)
		_boundTo->blockSignals(true);
	_boundTo->setValue(_groups);
	if (noSignal)
		_boundTo->blockSignals(false, false);
}
