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
#include "analysisform.h"
#include "jaspcontrol.h"
#include "jasplistcontrol.h"
#include "sourceitem.h"

#include "log.h"

#include <QQmlContext>

RowControls::RowControls(ListModel* parent
						 , QQmlComponent* component)
 : QObject(parent), _parentModel(parent), _rowComponent(component)
{
}

// Cannot do this code in the constructor: the Component create function (comp->create(context)) will call the addJASPControl method in JASPControl (or ListView),
// So this RowControls instance needs to exist already.
void RowControls::initValues(int row, const Term& key, const QMap<QString, Json::Value>& rowValues)
{
	JASPTIMER_SCOPE(RowControls::initValues);
	
	JASPListControl* listView = _parentModel->listView();

	JASPTIMER_START(RowControls::initValues context);
	_context = new QQmlContext(qmlContext(listView), this);
	_context->setContextProperty("isDynamic",	true);
	_context->setContextProperty("form",		listView->form());
	_context->setContextProperty("listView",	listView);
	_context->setContextProperty("isNew",		rowValues.empty());
	_context->setContextProperty("rowIndex",	row);
	_context->setContextProperty("rowLabel",	key.label());
	_context->setContextProperty("rowValue",	key.value());
	_context->setContextProperty("rowType",		columnTypeToQString(key.type()));
	JASPTIMER_STOP(RowControls::initValues context);
	
	JASPTIMER_START(RowControls::initValues create rowobject);
	_rowObject = qobject_cast<QQuickItem*>(_rowComponent->create(_context)); // The _rowJASPControlMap will be filled during this step
	assert(_rowObject);
	if (!_rowObject)
	{
		Log::log() << "Could not create control in " << listView->name() << std::endl;
		return;
	}
	JASPTIMER_STOP(RowControls::initValues create rowobject);

	
	JASPTIMER_START(RowControls::initValues setParent);
	_rowObject->setParent(_parentModel);
	JASPTIMER_STOP(RowControls::initValues setParent);

	JASPTIMER_START(RowControls::initValues setup control);
	QList<JASPControl*> controls = _rowJASPControlMap.values();
	for (JASPControl* control : controls)
		control->setUp();
	JASPTIMER_STOP(RowControls::initValues setup control);

	_initialized = true;
	emit initializedChanged();

	JASPTIMER_START(RowControls::initValues _setValues);
	_setValues(rowValues);
	JASPTIMER_STOP(RowControls::initValues _setValues);
}

void RowControls::resetValues(int row, const Term &key, const QMap<QString, Json::Value>& rowValues)
{
	JASPTIMER_SCOPE(RowControls::resetValues);
	
	// Cannot use qmlContext(item) : setContextProperty would generate: 'Cannot set property on internal context.' error
	_context->setContextProperty("rowIndex", row);
	_context->setContextProperty("rowLabel", key.label());
	_context->setContextProperty("rowValue", key.value());
	_context->setContextProperty("rowType", columnTypeToQString(key.type()));
	_context->setContextProperty("isNew", false);

	_setValues(rowValues);
}

void RowControls::_setValues(const QMap<QString, Json::Value>& rowValues)
{
	JASPTIMER_SCOPE(RowControls::_setValues);
	
	// The controls (when created or reused) need to be initialized
	QList<JASPControl*> controls = _rowJASPControlMap.values();
	JASPListControl* parentControl = _parentModel->listView();
	AnalysisForm* form = parentControl->form();


	if (form)
		form->sortControls(controls);

	for (JASPControl* control : controls)
	{
		JASPListControl* listView = dynamic_cast<JASPListControl*>(control);
		if (listView)
			for (SourceItem* source : listView->sourceItems())
				source->connectModels(); // If the source was disconnected, reconnect it.

		Json::Value optionValue = Json::nullValue;

		if (rowValues.contains(control->name()))
			optionValue = rowValues[control->name()];
		else
		{
			// It it exists, reuse the current value.
			BoundControl* boundItem = control->boundControl();
			if (boundItem)
				optionValue = boundItem->boundValue();
		}

		control->setInitialized(optionValue);
	}

	if (form)
		// setInitialized binds value to the control, but does not signal the change. So we have to manually emit the signal
		emit parentControl->boundValueChanged(parentControl);
}

bool RowControls::addJASPControl(JASPControl *control)
{
	JASPTIMER_SCOPE(RowControls::addJASPControl);
	
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

void RowControls::disconnectAndDeleteControls()
{
	JASPTIMER_SCOPE(RowControls::disconnectAndDeleteControls);
	
	// If a control depends on a source, disconnect this source with this control.
	for (JASPControl* control : _rowJASPControlMap.values())
	{
		JASPListControl* listControl = qobject_cast<JASPListControl*>(control);
		if (listControl)
			for (SourceItem* source : listControl->sourceItems())
				source->disconnectModels();
		
		control->cleanUp();
		control->setParent(nullptr);
		control->setUnitialized();
		control->blockSignals(true);
		control->deleteLater();
	}
}
