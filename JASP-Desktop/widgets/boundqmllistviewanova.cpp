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

#include "boundqmllistviewanova.h"
#include "analysis/analysisqmlform.h"
#include "listmodeltermsavailable.h"
#include <QQmlProperty>

using namespace std;

BoundQMLListViewAnova::BoundQMLListViewAnova(QQuickItem* item, AnalysisQMLForm* form) : BoundQMLDraggableListView(item, form)
{
	_boundTo = NULL;
	_model = _targetModel = _anovaModel = new ListModelAnovaAssigned(form, item);
	
	connect(_targetModel, &ListModelAnovaAssigned::termsChanged, this, &BoundQMLListViewAnova::modelChangedHandler);			
}

void BoundQMLListViewAnova::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionsTable *>(option);
	_anovaModel->initTerms(_boundTo->value(), _boundTo->rowTemplate());
}

void BoundQMLListViewAnova::unbind()
{
	
}

Option* BoundQMLListViewAnova::createOption()
{
	OptionTerm* optionTerm = new OptionTerm();
	Options* options = new Options();
	options->add("components", optionTerm);
	return new OptionsTable(options);	
}

void BoundQMLListViewAnova::modelChangedHandler()
{
	std::vector<Options *> rows = _anovaModel->rows();
	_boundTo->setValue(rows);
}
