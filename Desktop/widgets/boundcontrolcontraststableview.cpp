//
// Copyright (C) 2013-2021 University of Amsterdam
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

#include "boundcontrolcontraststableview.h"
#include "tableviewbase.h"
#include "listmodelcustomcontrasts.h"

BoundControlContrastsTableView::BoundControlContrastsTableView(TableViewBase* tableView)
	: BoundControlTableView (tableView)
{
}

Json::Value BoundControlContrastsTableView::createJson()
{
	Json::Value result = Json::Value(Json::arrayValue);
	ListModelCustomContrasts* contrastsModel = qobject_cast<ListModelCustomContrasts*>(_tableView->tableModel());


	QStringList variables;
	QVector<QVector<QVariant> > allLables;

	contrastsModel->getVariablesAndLabels(variables, allLables);

	if (variables.length() > 0)
	{
		Json::Value rowNames(Json::arrayValue);

		for (int row = 0; row < allLables[0].length(); row++)
			rowNames.append(fq(contrastsModel->getDefaultRowName(size_t(row))));

		int col = 0;
		for (const QString& variable : variables)
		{
			Json::Value row(Json::objectValue);
			row["name"] = fq(variable);
			row["levels"] = rowNames;
			row["isContrast"] = false;

			Json::Value values(Json::arrayValue);
			for (const QVariant & label: allLables[col])
				values.append(fq(label.toString()));
			row["values"] = values;

			col++;

			result.append(row);
		}

		if (_tableView->initialColumnCount() > 0)
		{
			Json::Value defaultValue = _defaultValue();

			for (int colIndex = 0; colIndex < _tableView->initialColumnCount(); colIndex++)
			{
				Json::Value row(Json::objectValue);
				row["name"] = fq(contrastsModel->getDefaultColName(size_t(variables.length() + colIndex)));
				row["levels"] = rowNames;
				row["isContrast"] = true;

				Json::Value values(Json::arrayValue);
				for (size_t i = 0; i < rowNames.size(); i++)
					values.append(defaultValue);
				row["values"] = values;

				result.append(row);
			}
		}
	}

	return result;
}

void BoundControlContrastsTableView::fillTableTerms(const Json::Value &value, ListModelTableViewBase::TableTerms &tableTerms)
{
	BoundControlTableView::fillTableTerms(value, tableTerms);

	int index = 0;
	for (const Json::Value& row : value)
	{
		if (!row["isContrast"].asBool())
			tableTerms.variables.push_back(tableTerms.colNames[index]);
		index++;
	}
}

void BoundControlContrastsTableView::fillBoundValue(Json::Value& value, const ListModelTableViewBase::TableTerms& tableTerms)
{
	BoundControlTableView::fillBoundValue(value, tableTerms);

	int i = 0;
	for (Json::Value& row : value)
	{
		row["isContrast"] = (i >= tableTerms.variables.length());
		i++;
	}
}

