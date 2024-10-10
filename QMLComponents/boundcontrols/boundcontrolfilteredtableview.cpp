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
	setIsColumn(true);
}

Json::Value BoundControlFilteredTableView::createJson() const
{
	Json::Value result(Json::arrayValue);
	Json::Value row(Json::objectValue);

	row["colName"]		= fq(_tableView->property("colName").toString());
	row["filter"]		= fq(_tableView->property("filter").toString());
	row["filterName"]	= fq(_tableView->property("filterName").toString());
	row["extraCol"]		= fq(_tableView->property("extraCol").toString());

	result.append(row);
	return result;
}

Json::Value BoundControlFilteredTableView::createMeta() const
{
	ListModelFilteredDataEntry* filteredModel = qobject_cast<ListModelFilteredDataEntry*>(_tableView->tableModel());
	
	Json::Value meta = BoundControlTableView::createMeta(),
				load = Json::objectValue;
	
	load["filter"] = filteredModel->filterName();
	load["column"] = fq(filteredModel->colName());
	
	meta["loadFilteredData"] = load;
	
	return meta;
}

void BoundControlFilteredTableView::fillTableTerms(const Json::Value &value, ListModelTableViewBase::TableTerms &tableTerms)
{
	if (value.size() > 0)
	{
		const Json::Value& firstRow = value[Json::UInt(0)];

		tableTerms.filter		= tq(firstRow["filter"].asString());
		tableTerms.filterName	= firstRow.isMember("filterName") ? tq(firstRow["filterName"].asString()) : "";
		tableTerms.colName		= tq(firstRow["colName"].asString());

		for (const Json::Value& value : firstRow["dataCols"])
			tableTerms.colNames.push_back(tq(value.asString()));

		const Json::Value& extraCol = firstRow["extraCol"];
		if (extraCol.isString())
			tableTerms.extraCol = tq(extraCol.asString());
		else if (extraCol.isArray() && extraCol.size() > 0)
			tableTerms.extraCol = tq(extraCol[Json::UInt(0)].asString());
	}
}

void BoundControlFilteredTableView::fillBoundValue(Json::Value &value, const ListModelTableViewBase::TableTerms &tableTerms)
{
	ListModelFilteredDataEntry* filteredModel = qobject_cast<ListModelFilteredDataEntry*>(_tableView->tableModel());

	Json::Value row(Json::objectValue);

	row["colName"]		= fq(filteredModel->colName()	);
	row["filter"]		= fq(filteredModel->filter()	);
	row["filterName"]	=	 filteredModel->filterName() ;


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
