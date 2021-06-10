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

#include "boundcontrolgridtableview.h"
#include "tableviewbase.h"

BoundControlGridTableView::BoundControlGridTableView(TableViewBase* tableView)
	: BoundControlTableView(tableView)
{
}

Json::Value BoundControlGridTableView::createJson()
{
	Json::Value result(Json::arrayValue);

	ListModelTableViewBase* model = _tableView->tableModel();
	Terms terms = model->getSourceTerms();

	Json::Value defaultValue = _defaultValue();

	int maxColumn = _tableView->initialColumnCount();
	if (_tableView->minColumn() > maxColumn)									maxColumn = _tableView->minColumn();
	if (_tableView->maxColumn() >= 0 && maxColumn > _tableView->maxColumn())	maxColumn = _tableView->maxColumn();
	int maxRow = _tableView->initialRowCount();
	if (_tableView->minRow() > maxRow)											maxRow = _tableView->minRow();
	if (_tableView->maxRow() >= 0 && maxRow > _tableView->maxRow())				maxRow = _tableView->maxRow();

	QList<QStringList> initValues;
	for (const Term& term : terms)
	{
		const QStringList& rowValues = term.components();
		if (rowValues.length() > maxRow)										maxRow = rowValues.length();
		initValues.append(rowValues);
	}
	if (initValues.length() > maxColumn)										maxColumn = initValues.length();
	if (_tableView->maxColumn() >= 0 && maxColumn > _tableView->maxColumn())	maxColumn = _tableView->maxColumn();
	if (_tableView->maxRow() >= 0 && maxRow > _tableView->maxRow())				maxRow = _tableView->maxRow();

	for (int rowNb = 0; rowNb < maxRow; rowNb++)
	{
		Json::Value levels(Json::arrayValue);
		Json::Value values(Json::arrayValue);

		for (int colNb = 0; colNb < maxColumn; colNb++)
		{
			if (initValues.length() > colNb && initValues.at(colNb).length() > rowNb)	values.append(fq(initValues.at(colNb).at(rowNb)));
			else																		values.append(defaultValue);
			levels.append(fq(_tableView->tableModel()->getDefaultColName(size_t(colNb))));
		}

		Json::Value row(Json::objectValue);
		row["levels"] = levels;
		row["name"] = fq(_tableView->tableModel()->getDefaultRowName(size_t(rowNb)));
		row["values"] = values;
		result.append(row);
	}

	return result;
}
