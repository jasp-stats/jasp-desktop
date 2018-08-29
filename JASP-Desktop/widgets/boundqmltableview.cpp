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

#include "boundqmltableview.h"
#include "analysis/analysisqmlform.h"

#include <QQmlProperty>

BoundQMLTableView::BoundQMLTableView(QQuickItem *item, AnalysisQMLForm *form)
	: BoundQMLItem(item, form)
	, _needsSyncModels(true)
{	
}

void BoundQMLTableView::setUp()
{
	BoundQMLItem::setUp();
	_model->setUp();
	
	if (_needsSyncModels)
	{
		QStringList syncModels = QQmlProperty(_item, "syncModels").read().toStringList();
		if (syncModels.isEmpty())
		{
			addError(QString::fromLatin1("Needs sync model for VariablesTable ") + _name);
		}
		else
		{
			for (const QString& syncModel : syncModels)
			{
				ListModel* model = _form->getModel(syncModel);
				if (model)
				{
					_syncModels.push_back(model);
					connect(model, &ListModel::termsChanged, this, &BoundQMLTableView::syncTermsChanged);
				}
				else
					addError(QString::fromLatin1("Unknown sync model ") + syncModel + QString::fromLatin1(" for ModelVIew ") + _name);
			}
			resetTermsFromSyncModels();
		}
	}
	
}

void BoundQMLTableView::syncTermsChanged(Terms *termsAdded, Terms *termsRemoved)
{
	Q_UNUSED(termsAdded);
	Q_UNUSED(termsRemoved);
	
	resetTermsFromSyncModels();
}
