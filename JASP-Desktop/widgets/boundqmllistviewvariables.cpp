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

#include "boundqmllistviewvariables.h"
#include "analysis/options/optionvariable.h"
#include "analysis/analysisqmlform.h"
#include "listmodeltermsavailable.h"

#include <QQmlProperty>

using namespace std;

BoundQMLListViewVariables::BoundQMLListViewVariables(QQuickItem* item, AnalysisQMLForm* form) : BoundQMLDraggableListView(item, form)
{
	_boundTo = NULL;
	_singleItem = QQmlProperty(item, "singleItem").read().toBool();
	_model = _targetModel = new ListModelTermsAssigned(form, item, _singleItem);
	
	connect(_targetModel, &ListModelTermsAssigned::termsChanged, this, &BoundQMLListViewVariables::modelChangedHandler);	
}

void BoundQMLListViewVariables::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionVariables *>(option);
	
	ListModelTermsAssigned* model = dynamic_cast<ListModelTermsAssigned *>(_targetModel);
	model->initTerms(_boundTo->value());
}

void BoundQMLListViewVariables::unbind()
{
	
}

Option* BoundQMLListViewVariables::createOption()
{
	OptionVariables *result = _singleItem ? new OptionVariable() : new OptionVariables();
	
	return result;
}

void BoundQMLListViewVariables::modelChangedHandler()
{
	_boundTo->setValue(_targetModel->terms().asVectorOfVectors());
}
