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

#include <QTimer>

using namespace std;


BoundQMLListViewMeasuresCells::BoundQMLListViewMeasuresCells(JASPControl* item)
	: JASPControlWrapper(item)
	, BoundQMLListViewDraggable(item)
{
	_needsSourceModels = true;	
	_measuresCellsModel = new ListModelMeasuresCellsAssigned(this);	
}

void BoundQMLListViewMeasuresCells::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionVariables *>(option);
	_boundTo->setShouldEncode(true);
	_measuresCellsModel->initLevels(getLevels(), _boundTo->value(), true);
}

const Terms& BoundQMLListViewMeasuresCells::getLevels()
{
	_tempTerms.clear();
	for (ListModelRepeatedMeasuresFactors* factorsModel : _sourceFactorsModels)
		_tempTerms.add(factorsModel->getLevels());
	
	
	return _tempTerms;
}

Option* BoundQMLListViewMeasuresCells::createOption()
{
	OptionVariables *result = new OptionVariables();
	result->setShouldEncode(true);
	result->setValue(vector<string>(getLevels().size(), ""));
	
	return result;
}

bool BoundQMLListViewMeasuresCells::isOptionValid(Option *option)
{
	return dynamic_cast<OptionVariables*>(option) != nullptr;
}

bool BoundQMLListViewMeasuresCells::isJsonValid(const Json::Value &optionValue)
{
	return optionValue.type() == Json::arrayValue;
}


void BoundQMLListViewMeasuresCells::setUp()
{
	BoundQMLListViewDraggable::setUp();
	
	for (SourceType* sourceItem : _sourceModels)
	{
		ListModelRepeatedMeasuresFactors* factorsModel = dynamic_cast<ListModelRepeatedMeasuresFactors*>(sourceItem->model);
		if (!factorsModel)
			addControlError(tr("Source model of %1 must be from a Factor List").arg(name()));
		addDependency(factorsModel->listView());
		_sourceFactorsModels.push_back(factorsModel);
	}
}

void BoundQMLListViewMeasuresCells::modelChangedHandler()
{
	const Terms& terms = _measuresCellsModel->terms();
	
	if (_boundTo)
	{
		_boundTo->setShouldEncode(true);
		_boundTo->setValue(terms.asVector());
	}
}
