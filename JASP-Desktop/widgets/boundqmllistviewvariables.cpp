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

BoundQMLListViewVariables::BoundQMLListViewVariables(QQuickItem* item, AnalysisQMLForm* form) : BoundQMLListView(item, form)
{
	_boundTo = NULL;
	_targetModel = new ListModelTermsAssigned(form, item);
	_singleItem = QQmlProperty(item, "singleItem").read().toBool();
}

void BoundQMLListViewVariables::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionVariables *>(option);

	if (_boundTo != NULL)
		_targetModel->bindTo(_boundTo);
	else
		qDebug() << "could not bind to OptionVariables in BoundQuickListView.cpp";
	
}

void BoundQMLListViewVariables::unbind()
{
	
}

Option* BoundQMLListViewVariables::createOption()
{
	OptionVariables *result = _singleItem ? new OptionVariable() : new OptionVariables();
	
	return result;
}
