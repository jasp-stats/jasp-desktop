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

#include "rowcontrols.h"
#include "analysis/analysisform.h"
#include "analysis/jaspcontrolbase.h"

#include "log.h"

#include <QQmlContext>

RowControls::RowControls(
	ListModel* parent
	, QVector<QQmlComponent *> &components
	, const QMap<QString, Option*>& rowOptions
	, int row
	, const QString& key)
	: QObject(parent), _parentModel(parent)
{
	QMLListView* listView = _parentModel->listView();
	int col = 0;
	for (QQmlComponent* comp : components)
	{
		QQmlContext* context = new QQmlContext(qmlContext(listView->item()), listView->item());
		context->setContextProperty("hasContextForm", true);
		context->setContextProperty("form", listView->form());
		context->setContextProperty("listView", listView);
		context->setContextProperty("colIndex", col);
		context->setContextProperty("isNew", true);
		context->setContextProperty("fromRowComponents", _rowControlsVarMap);
		context->setContextProperty("rowIndex",	row);
		context->setContextProperty("rowValue", key);
		QObject* obj = comp->create(context);
		if (obj)
		{
			JASPControlBase* control = dynamic_cast<JASPControlBase*>(obj);
			if (control)
			{
				if (control->name().isEmpty())
					listView->addError(tr("A row component in %1 does not have a name").arg(listView->name()));
				else if (_rowControlsVarMap.contains(control->name()))
					listView->addError(tr("2 row components in %1 have the same name").arg(listView->name()).arg(control->name()));
				else
				{
					_rowControls.push_back(QVariant::fromValue(control));
					_contextMap[control->name()] = context;
					_rowControlsVarMap[control->name()] = QVariant::fromValue(control);
					JASPControlWrapper* controlWrapper = control->getWrapper();
					if (controlWrapper)
					{
						_rowControlsMap[control->name()] = controlWrapper;
						BoundQMLItem* boundItem = dynamic_cast<BoundQMLItem*>(controlWrapper);
						if (boundItem)
						{
							Option* option = rowOptions.contains(boundItem->name()) ? rowOptions[boundItem->name()] : boundItem->createOption();
							boundItem->bindTo(option);
						}
					}
					else
						Log::log() << "A JASP Control (name: " << control->name() << ") has no wrapper" << std::endl;
				}
			}
			else
				Log::log() << "A row component in " << listView->name() << " is not a JASPControl" << std::endl;
		}
		else
			Log::log() << "Could not create control in ListView " << listView->name() << std::endl;
		col++;
	}
}

void RowControls::setContext(int row, const QString &key)
{
	for (JASPControlWrapper* controlWrapper : _rowControlsMap.values())
	{
		QQmlContext* context = _contextMap[controlWrapper->name()];
		context->setContextProperty("rowIndex",	row);
		context->setContextProperty("rowValue", key);
		context->setContextProperty("isNew", false);
		controlWrapper->item()->setParent(nullptr);
	}
}
