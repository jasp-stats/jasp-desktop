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

#include "boundcontrolfilteredtableview.h"
#include "controls/tableviewbase.h"
#include "models/listmodelfiltereddataentry.h"

BoundControlFilteredTableView::BoundControlFilteredTableView(TableViewBase* tableView)
	: BoundControlTableView(tableView)
{
}

Json::Value BoundControlFilteredTableView::createJson()
{
	Json::Value result(Json::arrayValue);
	Json::Value row(Json::objectValue);
	Json::Value values(Json::arrayValue);

	row["colName"]	= fq(_tableView->property("colName").toString());
	row["filter"]	= fq(_tableView->property("filter").toString());
	row["extraCol"] = fq(_tableView->property("extraCol").toString());
	row["values"] = values;

	result.append(row);
	return result;
}

void BoundControlFilteredTableView::fillTableTerms(const Json::Value &value, ListModelTableViewBase::TableTerms &tableTerms)
{
	if (value.size() > 0)
	{
		const Json::Value& firstRow = value[Json::UInt(0)];

		tableTerms.filter	= tq(firstRow["filter"].asString());

		tableTerms.values.push_back({});
		for (const Json::Value& value : firstRow["values"])
			tableTerms.values[0].push_back(value.asDouble());

		tableTerms.colName	= tq(firstRow["colName"].asString());

		for (const Json::Value& value : firstRow["dataCols"])
			tableTerms.colNames.push_back(tq(value.asString()));

		const Json::Value& extraCol = firstRow["extraCol"];
		if (extraCol.isString())
			tableTerms.extraCol = tq(extraCol.asString());
		else if (extraCol.isArray() && extraCol.size() > 0)
			tableTerms.extraCol = tq(extraCol[Json::UInt(0)].asString());

		for (const Json::Value& value : firstRow["rowIndices"])
			tableTerms.rowIndices.push_back(value.asInt());
	}
}

void BoundControlFilteredTableView::fillBoundValue(Json::Value &value, const ListModelTableViewBase::TableTerms &tableTerms)
{
	ListModelFilteredDataEntry* filteredModel = qobject_cast<ListModelFilteredDataEntry*>(_tableView->tableModel());

	Json::Value row(Json::objectValue);

	Json::Value stdRowIndices(Json::arrayValue);
	for (size_t index : filteredModel->filteredRowToData())
		stdRowIndices.append(static_cast<int>(index + 1));

	std::string colName = fq(filteredModel->colName());
	if (!colName.empty())
		row["colName"] = colName;
	row["filter"] =	fq(filteredModel->filter());
	row["rowIndices"] = stdRowIndices;

	Json::Value values(Json::arrayValue);
	for (QVariant val : tableTerms.values[0])
		values.append(val.toDouble());
	row["values"] = values;

	Json::Value dataCols(Json::arrayValue);
	for (const QString& dataCol : filteredModel->dataColumns())
		dataCols.append(fq(dataCol));
	row["dataCols"] = dataCols;

	Json::Value extraColJson(Json::arrayValue);
	std::string extraCol = fq(filteredModel->extraCol());
	if (!extraCol.empty())
		extraColJson.append(extraCol);
	row["extraCol"] = extraColJson;

	value.append(row);
}
