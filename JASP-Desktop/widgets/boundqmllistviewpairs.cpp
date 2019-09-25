//
// Copyright (C) 2013-2018 University of Amsterdam
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

#include "boundqmllistviewpairs.h"
#include "listmodelpairsassigned.h"
#include "../analysis/analysisform.h"
#include "analysis/options/optionvariablesgroups.h"

#include <QQmlProperty>

using namespace std;

BoundQMLListViewPairs::BoundQMLListViewPairs(QQuickItem* item, AnalysisForm* form) 
	: QMLItem(item, form)
	, BoundQMLListViewDraggable(item, form)
	, _boundTo(nullptr)
{
	_variableTypesSuggested = int(columnType::nominal) | int(columnType::ordinal) | int(columnType::scale);
	_pairsModel				= new ListModelPairsAssigned(this);

	setDropMode(qmlDropMode::Replace);
	QQmlProperty::write(_item, "showElementBorder",		true);
	QQmlProperty::write(_item, "columns",				2);
	QQmlProperty::write(_item, "showVariableTypeIcon",	false);
}

void BoundQMLListViewPairs::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionVariablesGroups *>(option);
	_pairsModel->initTerms(_boundTo->value());
}

Option* BoundQMLListViewPairs::createOption()
{
	OptionVariablesGroups *result = new OptionVariablesGroups();
	
	return result;
}

bool BoundQMLListViewPairs::isOptionValid(Option *option)
{
	return dynamic_cast<OptionVariablesGroups*>(option) != nullptr;
}

bool BoundQMLListViewPairs::isJsonValid(const Json::Value &optionValue)
{
	bool ok = optionValue.type() == Json::arrayValue;

	return ok;
}

void BoundQMLListViewPairs::modelChangedHandler()
{
	vector<vector<string> > pairs;

	for (const Term &qPair : _pairsModel->terms())
	{
		vector<string> pair;
		pair.push_back(qPair.at(0).toStdString());
		pair.push_back(qPair.at(1).toStdString());
		pairs.push_back(pair);
	}

	_boundTo->setValue(pairs);
}
