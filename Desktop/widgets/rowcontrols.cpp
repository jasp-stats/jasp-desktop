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
#include "jasplistcontrol.h"

#include "log.h"

#include <QQmlContext>

RowControls::RowControls(ListModel* parent
						 , QQmlComponent* component
						 , const QMap<QString, Option*>& rowOptions
						 , bool isDummy)
 : QObject(parent), _parentModel(parent), _rowComponent(component), _rowOptions(rowOptions), _isDummy(isDummy)
{
}

// Cannot do this code in the constructor: the Component create function (comp->create(context)) will call the addJASPControl method in JASPControl (or ListView),
// So this RowControls instance needs to exist already.
void RowControls::init(int row, const Term& key, bool isNew)
{
	JASPListControl* listView = _parentModel->listView();

	QQmlContext* context = new QQmlContext(qmlContext(listView), listView);
	if (_isDummy)
		context->setContextProperty("noDirectSetup", true);
	context->setContextProperty("isDynamic", true);
	context->setContextProperty("form", listView->form());
	context->setContextProperty("listView", listView);
	context->setContextProperty("isNew", isNew);
	context->setContextProperty("rowIndex",	row);
	context->setContextProperty("rowValue", key.asQString());

	_rowObject = qobject_cast<QQuickItem*>(_rowComponent->create(context));

	if (_rowObject)
	{
		_context = context;

		if (_isDummy) // A dummy will never be used in QML, and does not get a parent, but a parent is needed to destroy it
			_rowObject->setParent(this);
	}
	else
		Log::log() << "Could not create control in ListView " << listView->name() << std::endl;

}

void RowControls::setContext(int row, const QString &key)
{
	// Cannot use qmlContext(item) : setContextProperty would generate: 'Cannot set property on internal context.' error
	_context->setContextProperty("rowIndex",	row);
	_context->setContextProperty("rowValue", key);
	_context->setContextProperty("isNew", false);
	_rowObject->setParentItem(nullptr);
}

bool RowControls::addJASPControl(JASPControl *control)
{
	bool success = false;
	JASPListControl* listView = _parentModel->listView();

	if (!control->isBound())
		success = true;
	else if (control->name().isEmpty())
		listView->addControlError(tr("A row component in %1 does not have a name").arg(listView->name()));
	else if (_rowControlsVarMap.contains(control->name()))
		listView->addControlError(tr("2 row components in %1 have the same name").arg(listView->name()).arg(control->name()));
	else
	{
		QQmlContext* context = qmlContext(control);
		bool isDummy = context->contextProperty("noDirectSetup").toBool();

		_rowControlsVarMap[control->name()] = QVariant::fromValue(control);
		_rowJASPControlMap[control->name()] = control;
		BoundControl* boundItem = dynamic_cast<BoundControl*>(control);

		if (control->isBound() && boundItem && !isDummy)
		{
			bool hasOption = _rowOptions.contains(control->name());
			Option* option =  hasOption ? _rowOptions[control->name()] : boundItem->createOption();

			boundItem->bindTo(option);
			if (!hasOption)
			{
				JASPListControl* listView = dynamic_cast<JASPListControl*>(control);
				// If a ListView depends on a source, it has to be initialized by this source
				// For this just call the sourceTermsChanged handler.
				if (listView && listView->hasSource())
					listView->model()->sourceTermsReset();
			}
		}

		success = true;
	}

	return success;
}
