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

#include <QDebug>

BoundTableWidget::BoundTableWidget(QWidget *parent)
	: QTableWidget(parent)
{
	_boundTo = NULL;
}

void BoundTableWidget::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionsTable *>(option);
	qDebug() << "bindTo()";
	if (_boundTo != NULL) {
		_groups = _boundTo->value();
	}
}

void BoundTableWidget::updateTableValues()
{
	if (_boundTo == NULL) {
		return;
	}

	qDebug() << "updateTableValues()";


	int columnCount = this->columnCount();
	int rowCount = this->rowCount();

	_groups.clear();
	std::vector<std::string> verticalHeaders;

	// Get verticalHeaders
	for (int i = 0; i < rowCount; ++i) {
		QTableWidgetItem *verticalHeaderItem = this->verticalHeaderItem(i);
		QString header = "";
		if (verticalHeaderItem != NULL) {
			header = verticalHeaderItem->text();
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

		// OptionVariables *option = static_cast<OptionVariables *>(newRow->get("values"));
		OptionDoubleArray *option = static_cast<OptionDoubleArray *>(newRow->get("values"));
		OptionVariables *headers = static_cast<OptionVariables *>(newRow->get("levels"));
		headers->setValue(verticalHeaders);
		// std::vector<std::string> values;
		std::vector<double> values;
		std::vector<std::string> levels;

		for (int j = 0; j < rowCount; ++j) {
			QTableWidgetItem *rowItem = this->item(j, i);
			QString content = "";

			if (rowItem != NULL) {
				content = rowItem->text();
			}

			// values.push_back(fq(content));
			values.push_back(content.toDouble());
		}

		option->setValue(values);

		_groups.push_back(newRow);
	}

	// FIXME: Below sentence should be used
	_boundTo->setValue(_groups);
}
