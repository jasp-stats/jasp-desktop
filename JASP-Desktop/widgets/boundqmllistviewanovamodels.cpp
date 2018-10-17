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

#include "boundqmllistviewanovamodels.h"
#include "analysis/analysisqmlform.h"
#include "analysis/options/optionboolean.h"
#include "listmodeltermsavailable.h"
#include <QQmlProperty>

using namespace std;

BoundQMLListViewAnovaModels::BoundQMLListViewAnovaModels(QQuickItem* item, AnalysisQMLForm* form) 
	: QMLItem(item, form)
	, BoundQMLListViewDraggable(item, form)
{
	_boundTo = NULL;
	_anovaModel = new ListModelAnovaAssigned(this);
	setTermsAreNotVariables();		
}

void BoundQMLListViewAnovaModels::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionsTable *>(option);
	_anovaModel->initTermsWithTemplate(_boundTo->value(), _boundTo->rowTemplate());
}

void BoundQMLListViewAnovaModels::unbind()
{
	
}

Option* BoundQMLListViewAnovaModels::createOption()
{
	OptionTerm* optionTerm = new OptionTerm();
	Options* options = new Options();
	options->add("components", optionTerm);
	if (_hasExtraControlColumns)
	{
		QMapIterator<QString, QMap<QString, QString> > i(_extraControlColumns);
		while (i.hasNext())
		{
			i.next();
			const QMap<QString, QString>& properties = i.value();
			if (properties["type"] == "CheckBox")
			{
				OptionBoolean* optionBoolean = new OptionBoolean();
				options->add(i.key().toStdString(), optionBoolean);
			}
		}
	}
	return new OptionsTable(options);	
}

void BoundQMLListViewAnovaModels::modelChangedHandler()
{
	const std::vector<Options *>& rows = _anovaModel->rows();
	
	if (_hasExtraControlColumns)
	{
		for (Options* row : rows)
		{
			OptionTerms *termOption = static_cast<OptionTerms *>(row->get(0));
			Term t(termOption->value().front());
			QString termStr = t.asQString();
			const QMap<QString, BoundQMLItem*>& controlMap = _rowsWithControls[termStr];
			QMapIterator<QString, BoundQMLItem*> it(controlMap);
			while (it.hasNext())
			{
				it.next();
				QString optionName = it.key();
				BoundQMLItem* boundItem = it.value();
				Option* option = row->get(optionName.toStdString());
				if (option)
					boundItem->bindTo(option);
				else
					qDebug() << "No option found with name " << optionName;
			}
		}
	}
	
	_boundTo->connectOptions(rows);
}
