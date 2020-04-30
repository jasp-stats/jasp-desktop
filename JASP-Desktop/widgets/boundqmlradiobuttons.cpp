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

#include "boundqmlradiobuttons.h"
#include "analysis/jaspcontrolbase.h"
#include <QQmlProperty>
#include <QQuickItem>
#include "log.h"


using namespace std;

BoundQMLRadioButtons::BoundQMLRadioButtons(JASPControlBase* item)
	: JASPControlWrapper(item)
	, BoundQMLItem()
{
}

void BoundQMLRadioButtons::setUp()
{
	JASPControlWrapper::setUp();
	QList<JASPControlWrapper* > buttons;
	_getRadioButtons(item(), buttons);
	QVariant buttonGroup = getItemProperty("buttonGroup");

	for (JASPControlWrapper* button: buttons)
	{	
		const QString& controlName = button->name();
		if (controlName.isEmpty())
			addControlError(tr("A RadioButton inside RadioButtonGroup element (name: %1) does not have any name").arg(name()));
		else
		{
			_buttons[controlName] = button;
			bool checked = button->getItemProperty("checked").toBool();
			if (checked)
			{
				_checkedButton = button;
				setItemProperty("value", _checkedButton->name());
			}
			button->setItemProperty("buttonGroup", buttonGroup);
		}
	}

	if (!_checkedButton)
		Log::log() << "No checked button found in radio buttons " << name() << std::endl;

	QQuickItem::connect(item(), SIGNAL(clicked(const QVariant &)), this, SLOT(radioButtonClickedHandler(const QVariant &)));
}

void BoundQMLRadioButtons::_getRadioButtons(QQuickItem* item, QList<JASPControlWrapper* >& buttons) {
	for (QQuickItem* child : item->childItems())
	{
		JASPControlBase* jaspControl = dynamic_cast<JASPControlBase*>(child);
		if (jaspControl)
		{
			JASPControlBase::ControlType controlType = jaspControl->controlType();
			if (controlType == JASPControlBase::ControlType::RadioButton)
				buttons.append(jaspControl->getWrapper());
			else if (controlType != JASPControlBase::ControlType::RadioButtonGroup)
				_getRadioButtons(child, buttons);
		}
		else
			_getRadioButtons(child, buttons);
	}	
}

void BoundQMLRadioButtons::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionList *>(option);

	if (_boundTo == nullptr)
	{
		Log::log()  << "could not bind to OptionList in BoundQuickRadioButtons" << std::endl;
		return;
	}

	string value = _boundTo->value();
	if (!value.empty())
	{
		JASPControlWrapper* button = _buttons[QString::fromStdString(value)];
		if (!button)
		{
			addControlError(tr("No radio button corresponding to name %1").arg(QString::fromStdString(value)));
			QStringList names = _buttons.keys();
			Log::log()  << "Known button: " << names.join(',').toStdString() << std::endl;
		}
		else
		{
			button->setItemProperty("checked", true);
			_checkedButton = button;
			setItemProperty("value", _checkedButton->name());

		}
	}
}

void BoundQMLRadioButtons::unbind()
{
	
}

Option *BoundQMLRadioButtons::createOption()
{
	QString defaultValue = _checkedButton ? _checkedButton->getItemProperty("name").toString() : "";
	std::vector<std::string> options;
	for (QString value : _buttons.keys())
		options.push_back(value.toStdString());
	
	return new OptionList(options, defaultValue.toStdString());
}

bool BoundQMLRadioButtons::isOptionValid(Option *option)
{
	return dynamic_cast<OptionList*>(option) != nullptr;
}

bool BoundQMLRadioButtons::isJsonValid(const Json::Value &optionValue)
{
	return optionValue.type() == Json::stringValue;
}

void BoundQMLRadioButtons::radioButtonClickedHandler(const QVariant& button)
{
	QObject* objButton = button.value<QObject*>();
	if (objButton)
		objButton = objButton->parent();
	JASPControlBase *quickButton = qobject_cast<JASPControlBase*>(objButton);
	if (quickButton)
	{
		QString buttonName = quickButton->name();
		JASPControlWrapper* foundButton = _buttons[buttonName];
		if (foundButton)
		{
			if (_checkedButton != foundButton)
			{
				if (_checkedButton)
					_checkedButton->setItemProperty("checked",false);
				_checkedButton = foundButton;
				setItemProperty("value", _checkedButton->name());

				if (_boundTo)
					_boundTo->setValue(buttonName.toStdString());
			}
		}
		else
			addControlError(tr("Radio button clicked is unknown: %1").arg(buttonName));
	}
	else
		Log::log() << "Object clicked is not a quick item! Name" << objButton->objectName().toStdString();
}
