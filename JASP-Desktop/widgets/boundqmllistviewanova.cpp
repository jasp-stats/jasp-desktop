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

BoundQMLListViewAnova::BoundQMLListViewAnova(QQuickItem* item, AnalysisQMLForm* form) : BoundQMLListView(item, form)
{
	_boundTo = NULL;
	_targetModel = new ListModelAnovaAssigned(form, item);
	_targetModel->setShowVariableIcon(false);
}

void BoundQMLListViewAnova::setUp()
{
	BoundQMLListView::setUp();
	_sourceModel->setRemoveTermsWhenDropped(false);
	
}

void BoundQMLListViewAnova::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionsTable *>(option);

	if (_boundTo != NULL)
		_targetModel->bindTo(_boundTo);
	else
		qDebug() << "could not bind to OptionVariables in BoundQuickAnovaModelView.cpp";
	
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
