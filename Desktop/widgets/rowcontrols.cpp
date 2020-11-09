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
#include "analysis/jaspcontrol.h"
#include "qmllistview.h"

#include "log.h"

#include <QQmlContext>

RowControls::RowControls(ListModel* parent
						 , QList<QQmlComponent *>& components
						 , const QMap<QString, Option*>& rowOptions
						 , bool isDummy)
 : QObject(parent), _parentModel(parent), _rowComponents(components), _rowOptions(rowOptions), _isDummy(isDummy)
{
}

// Cannot do this code in the constructor: the Component create function (comp->create(context)) will call the addJASPControl method in JASPControl (or ListView),
// So this RowControls instance needs to exist already.
void RowControls::init(int row, const Term& key, bool isNew)
{
	QMLListView* listView = _parentModel->listView();
	int col = 0;
	for (QQmlComponent* comp : _rowComponents)
	{
		QQmlContext* context = new QQmlContext(qmlContext(listView->item()), listView->item());
		if (_isDummy)
			context->setContextProperty("noDirectSetup", true);
		context->setContextProperty("hasContextForm", true);
		context->setContextProperty("form", listView->form());
		context->setContextProperty("listView", listView);
		context->setContextProperty("colIndex", col);
		context->setContextProperty("isNew", isNew);
		context->setContextProperty("fromRowComponents", _rowControlsVarMap);
		context->setContextProperty("rowIndex",	row);
		context->setContextProperty("rowValue", key.asQString());
		context->setContextProperty("rowValueIsInteraction", key.components().size() > 1);

		QVariantMap prop;
		if (_isDummy)	prop["visible"] = false;
		QQuickItem* obj = qobject_cast<QQuickItem*>(comp->createWithInitialProperties(prop, context));

		if (obj)
		{
			_contextMap[obj] = context;
			_rowObjects.push_back(QVariant::fromValue(obj));

			if (_isDummy) // A dummy will never be used in QML, and does not get a parent, but a parent is needed to destroy it
				obj->setParent(this);
		}
		else
			Log::log() << "Could not create control in ListView " << listView->name() << std::endl;

		col++;
	}
}

void RowControls::setContext(int row, const QString &key)
{
	for (auto & itemContext : _contextMap)
	{
		// Cannot use qmlContext(item) : setContextProperty would generate: 'Cannot set property on internal context.' error
		itemContext.second->setContextProperty("rowIndex",	row);
		itemContext.second->setContextProperty("rowValue", key);
		itemContext.second->setContextProperty("isNew", false);
		itemContext.first->setParentItem(nullptr);
	}
}

bool RowControls::addJASPControl(JASPControlWrapper *control)
{
	bool success = false;
	QMLListView* listView = _parentModel->listView();

	if (!control->isBound())
		success = true;
	else if (control->name().isEmpty())
		listView->addControlError(tr("A row component in %1 does not have a name").arg(listView->name()));
	else if (_rowControlsVarMap.contains(control->name()))
		listView->addControlError(tr("2 row components in %1 have the same name").arg(listView->name()).arg(control->name()));
	else
	{
		QQmlContext* context = qmlContext(control->item());
		bool isDummy = context->contextProperty("noDirectSetup").toBool();

		_rowControlsVarMap[control->name()] = QVariant::fromValue(control->item());
		_rowJASPWrapperMap[control->name()] = control;
		BoundQMLItem* boundItem = dynamic_cast<BoundQMLItem*>(control);

		if (boundItem && !isDummy)
		{
			bool hasOption = _rowOptions.contains(boundItem->name());
			Option* option =  hasOption ? _rowOptions[boundItem->name()] : boundItem->createOption();

			boundItem->bindTo(option);
			if (!hasOption)
			{
				QMLListView* listView = dynamic_cast<QMLListView*>(boundItem);
				// If a ListView depends on a source, it has to be initialized by this source
				// For this just call the sourceTermsChanged handler.
				if (listView && listView->hasSource())
					listView->model()->sourceTermsChanged(nullptr, nullptr);
			}
		}

		success = true;
	}

	return success;
}
