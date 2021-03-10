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
						 , const QMap<QString, Json::Value>& rowValues)
 : QObject(parent), _parentModel(parent), _rowComponent(component), _rowValues(rowValues)
{
}

// Cannot do this code in the constructor: the Component create function (comp->create(context)) will call the addJASPControl method in JASPControl (or ListView),
// So this RowControls instance needs to exist already.
void RowControls::init(int row, const Term& key, bool isNew)
{
	JASPListControl* listView = _parentModel->listView();

	QQmlContext* context = new QQmlContext(qmlContext(listView), listView);
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

		QList<JASPControl*> controls = _rowJASPControlMap.values();

		for (JASPControl* control : controls)
			control->setUp();

		AnalysisForm* form = listView->form();
		if (form)
		{
			form->sortControls(controls);
			form->blockValueChangeSignal(true);
		}

		for (JASPControl* control : controls)
		{
			BoundControl* boundItem = control->boundControl();
			bool hasOption = _rowValues.contains(control->name());

			if (boundItem)
			{
				Json::Value option =  hasOption ? (_rowValues[control->name()]) : boundItem->createJson();
				boundItem->bindTo(option);
			}

			if (!boundItem || !hasOption)
			{
				JASPListControl* listView = dynamic_cast<JASPListControl*>(control);
				// If a ListView depends on a source, it has to be initialized by this source
				// For this just call the sourceTermsChanged handler.
				if (listView && listView->hasSource())
					listView->model()->sourceTermsReset();
			}

			control->setInitialized();
		}

		if (form)
			form->blockValueChangeSignal(false, false);

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

	if (control->isBound() && control->name().isEmpty())
		listView->addControlError(tr("A row component in %1 does not have a name").arg(listView->name()));
	else if (_rowJASPControlMap.contains(control->name()))
		listView->addControlError(tr("2 row components in %1 have the same name").arg(listView->name()).arg(control->name()));
	else
		success = true;

	if (!control->name().isEmpty() && success)
		_rowJASPControlMap[control->name()] = control;

	return success;
}
