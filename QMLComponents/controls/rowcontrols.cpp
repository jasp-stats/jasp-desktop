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
#include <QElapsedTimer>

RowControls::RowControls(ListModel* parent
						 , QQmlComponent* component)
 : QObject(parent), _parentModel(parent), _rowComponent(component)
{
}

// Cannot do this code in the constructor: the Component create function (comp->create(context)) will call the addJASPControl method in JASPControl (or ListView),
// So this RowControls instance needs to exist already.
void RowControls::initValues(int row, const Term& key, const QMap<QString, Json::Value>& rowValues)
{
	QElapsedTimer t; t.start();
	JASPListControl* listView = _parentModel->listView();

	QQmlContext* context = new QQmlContext(qmlContext(listView), this);
	context->setContextProperty("isDynamic", true);
	context->setContextProperty("form", listView->form());
	context->setContextProperty("listView", listView);
	context->setContextProperty("isNew", rowValues.empty());
	context->setContextProperty("rowIndex",	row);
	context->setContextProperty("rowLabel", key.label());
	context->setContextProperty("rowValue", key.value());
	context->setContextProperty("rowType", columnTypeToQString(key.type()));

	QElapsedTimer ct; ct.start();
	_rowObject = qobject_cast<QQuickItem*>(_rowComponent->create(context)); // The _rowJASPControlMap will be filled during this step
	qint64 createMs = ct.elapsed();

	assert(_rowObject);
	if (!_rowObject)
	{
		Log::log() << "Could not create control in " << listView->name() << std::endl;
		return;
	}

	_rowObject->setParent(_parentModel);
	_context = context;

	ct.restart();
	QList<JASPControl*> controls = _rowJASPControlMap.values();
	for (JASPControl* control : controls)
		control->setUp();
	qint64 setUpMs = ct.elapsed();

	_initialized = true;
	emit initializedChanged();

	ct.restart();
	_setValues(rowValues);
	qint64 setValMs = ct.elapsed();

	qint64 totalMs = t.elapsed();
	if (totalMs > 50)
		Log::log() << "[PERF] RowControls::initValues() row=" << row << " key=" << key.value() << " in " << listView->name()
				   << " took " << totalMs << "ms (create=" << createMs << "ms, setUp=" << setUpMs << "ms, setValues=" << setValMs << "ms, controls=" << controls.size() << ")" << std::endl;
}

void RowControls::resetValues(int row, const Term &key, const QMap<QString, Json::Value>& rowValues)
{
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
	QElapsedTimer t; t.start();

	// The controls (when created or reused) need to be initialized
	QList<JASPControl*> controls = _rowJASPControlMap.values();
	JASPListControl* parentControl = _parentModel->listView();
	AnalysisForm* form = parentControl->form();

	QElapsedTimer st; st.start();
	if (form)
		form->sortControls(controls);
	qint64 sortMs = st.elapsed();

	st.restart();
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
	qint64 initMs = st.elapsed();

	if (form)
		// setInitialized binds value to the control, but does not signal the change. So we have to manually emit the signal
		emit parentControl->boundValueChanged(parentControl);

	qint64 totalMs = t.elapsed();
	if (totalMs > 50)
		Log::log() << "[PERF] RowControls::_setValues() in " << parentControl->name() << " took " << totalMs << "ms (sort=" << sortMs << "ms, init=" << initMs << "ms, controls=" << controls.size() << ")" << std::endl;
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

void RowControls::disconnectAndDeleteControls()
{
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
