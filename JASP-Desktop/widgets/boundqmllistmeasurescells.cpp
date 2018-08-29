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

#include "boundqmllistmeasurescells.h"
#include "listmodelmeasurescells.h"
#include "listmodeldraggableterms.h"
#include "listmodelfactors.h"
#include "analysis/analysisqmlform.h"
#include "utilities/qutils.h"

#include <QQmlProperty>
#include <QTimer>

using namespace std;


BoundQMLListMeasuresCells::BoundQMLListMeasuresCells(QQuickItem* item, AnalysisQMLForm* form) : BoundQMLDraggableListView(item, form)
{
	_boundTo = NULL;
	_needsSyncModels = true;
	_model = _targetModel = _measuresCellsModel = new ListModelMeasuresCells(form, item);
	
	_measuresCellsModel->setDropMode(qmlDropMode::Replace);
	QQmlProperty::write(item, "showElementBorder", true);
	QQmlProperty::write(item, "columns", 2);
	QQmlProperty::write(item, "dragOnlyVariables", true);
	QQmlProperty::write(item, "showVariableIcon", false);	
	
	connect(_measuresCellsModel, &ListModelMeasuresCells::termsChanged, this, &BoundQMLListMeasuresCells::modelChangedHandler);			
}

void BoundQMLListMeasuresCells::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionVariables *>(option);
	resetTermsFromSyncModels();
	_measuresCellsModel->initVariables(_boundTo->value());
}

void BoundQMLListMeasuresCells::unbind()
{
	
}

void BoundQMLListMeasuresCells::resetTermsFromSyncModels()
{
	_tempTerms.clear();
	for (ListModelFactors* factorsModel : _syncFactorsModels)
	{
		const Terms& terms = factorsModel->getLevels();
		_tempTerms.add(terms);
	}
	
	_measuresCellsModel->initLevels(_tempTerms);
	
}

Option* BoundQMLListMeasuresCells::createOption()
{
	OptionVariables *result = new OptionVariables();
	
	return result;
}

void BoundQMLListMeasuresCells::setUp()
{
	BoundQMLDraggableListView::setUp();
	
	for (ListModel* model : _syncModels)
	{
		ListModelFactors* factorsModel = dynamic_cast<ListModelFactors*>(model);
		if (!factorsModel)
			addError(tq("Sync model of ") + name() + tq(" must be from a Factor List"));
		_syncFactorsModels.push_back(factorsModel);
	}
}

void BoundQMLListMeasuresCells::modelChangedHandler()
{
	const QList<QString>& variables = _measuresCellsModel->variables();
	vector<string> terms;
	for (const QString& variable : variables)
		terms.push_back(variable.toStdString());
	_boundTo->setValue(terms);
}
