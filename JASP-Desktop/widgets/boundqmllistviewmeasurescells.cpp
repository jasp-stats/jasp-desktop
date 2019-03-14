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

#include "boundqmllistviewmeasurescells.h"
#include "listmodelmeasurescellsassigned.h"
#include "listmodelrepeatedmeasuresfactors.h"
#include "../analysis/analysisform.h"
#include "utilities/qutils.h"

#include <QQmlProperty>
#include <QTimer>

using namespace std;


BoundQMLListViewMeasuresCells::BoundQMLListViewMeasuresCells(QQuickItem* item, AnalysisForm* form) 
	: QMLItem(item, form)
	, BoundQMLListViewDraggable(item, form)
{
	_boundTo = nullptr;
	_needsSourceModels = true;	
	_measuresCellsModel = new ListModelMeasuresCellsAssigned(this);
	
	setDropMode(qmlDropMode::Replace);
	QQmlProperty::write(_item, "showElementBorder", true);
	QQmlProperty::write(_item, "columns", 2);
	QQmlProperty::write(_item, "dragOnlyVariables", true);
	QQmlProperty::write(_item, "showVariableTypeIcon", false);		
}

void BoundQMLListViewMeasuresCells::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionVariables *>(option);
	_measuresCellsModel->initLevels(getLevels(), _boundTo->value(), true);
}

const Terms& BoundQMLListViewMeasuresCells::getLevels()
{
	_tempTerms.clear();
	for (ListModelRepeatedMeasuresFactors* factorsModel : _sourceFactorsModels)
	{
		const Terms& terms = factorsModel->getLevels();
		_tempTerms.add(terms);
	}
	
	return _tempTerms;
}

Option* BoundQMLListViewMeasuresCells::createOption()
{
	OptionVariables *result = new OptionVariables();
	vector<string> values;
	for (size_t i = 0; i < getLevels().size(); i++)
		values.push_back("");
	result->setValue(values);
	
	return result;
}

bool BoundQMLListViewMeasuresCells::isOptionValid(Option *option)
{
	return dynamic_cast<OptionVariables*>(option) != nullptr;
}

bool BoundQMLListViewMeasuresCells::isJsonValid(const Json::Value &optionValue)
{
	bool ok = optionValue.type() == Json::arrayValue;

	return ok;
}


void BoundQMLListViewMeasuresCells::setUp()
{
	BoundQMLListViewDraggable::setUp();
	
	for (SourceType* sourceItem : _sourceModels)
	{
		ListModelRepeatedMeasuresFactors* factorsModel = dynamic_cast<ListModelRepeatedMeasuresFactors*>(sourceItem->model);
		if (!factorsModel)
			addError(tq("Source model of ") + name() + tq(" must be from a Factor List"));
		addDependency(factorsModel->listView());
		_sourceFactorsModels.push_back(factorsModel);
	}
}

void BoundQMLListViewMeasuresCells::modelChangedHandler()
{
	const QList<QString>& variables = _measuresCellsModel->variables();
	vector<string> terms;
	for (const QString& variable : variables)
		terms.push_back(variable.toStdString());
	_boundTo->setValue(terms);
}
