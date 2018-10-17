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
#include <QQmlProperty>
#include <QQuickItem>
#include <QDebug>

using namespace std;

BoundQMLRadioButtons::BoundQMLRadioButtons(QQuickItem* item, AnalysisQMLForm* form)
	: QMLItem(item, form)
	, BoundQMLItem(item, form)
{
	_boundTo = NULL;
	_checkedButton = NULL;
	
	QList<QQuickItem* > buttons;
	_getRadioButtons(item, buttons);
	
	for (QQuickItem* button: buttons)
	{	
		QString controlName = QQmlProperty(button, "name").read().toString();
		if (controlName.isEmpty())
			addError(QString::fromLatin1("A RadioButton inside ButtonGroup element (name: ") + name() + QString::fromLatin1(") does not have any name"));
		else
		{
			_buttons[controlName.toStdString()] = button;
			bool checked = QQmlProperty(button, "checked").read().toBool();
			if (checked)
				_checkedButton = button;
		}
	}

	QQuickItem::connect(item, SIGNAL(clicked(const QVariant &)), this, SLOT(radioButtonClickedHandler(const QVariant &)));
}

void BoundQMLRadioButtons::_getRadioButtons(QQuickItem* item, QList<QQuickItem* >& buttons) {
	for (QQuickItem* child : item->childItems())
	{
		QString controlType = QQmlProperty(child, "controlType").read().toString();
		if (controlType == "RadioButton")
			buttons.append(child);
		else if (controlType != "ButtonGroup")
			_getRadioButtons(child, buttons);
	}	
}

void BoundQMLRadioButtons::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionString *>(option);

	if (_boundTo == NULL)
	{
		qDebug() << "could not bind to OptionString in BoundQuickRadioButtons";
		return;
	}

	string value = _boundTo->value();
	if (!value.empty())
	{
		QQuickItem* button = _buttons[value];
		if (!button)
		{
			addError(QString::fromLatin1("No radio button corresponding to name ") + QString::fromStdString(value));
			for (const auto &pair : _buttons)
				qDebug() << "Known button: " << QString::fromStdString(pair.first);
		}
		else
			QQmlProperty(_item, "checkedButton").write(QVariant::fromValue(button));
	}
	
}

void BoundQMLRadioButtons::unbind()
{
	
}

Option *BoundQMLRadioButtons::createOption()
{
	QString defaultValue = _checkedButton ? QQmlProperty(_checkedButton, "name").read().toString() : "";
	return new OptionString(defaultValue.toStdString());
}

void BoundQMLRadioButtons::radioButtonClickedHandler(const QVariant& button)
{
	QObject* objButton = button.value<QObject*>();
	if (objButton)
		objButton = objButton->parent();
	QQuickItem *quickButton = qobject_cast<QQuickItem*>(objButton);
	if (quickButton)
	{
		QString buttonName = QQmlProperty(quickButton, "name").read().toString();
		QQuickItem* foundButton = _buttons[buttonName.toStdString()];
		if (foundButton)
		{
			if (_checkedButton != foundButton)
			{
				QQmlProperty(_checkedButton, "checked").write(false);
				_checkedButton = foundButton;
				if (_boundTo)
					_boundTo->setValue(buttonName.toStdString());
			}
		}
		else
		{
			addError(QString::fromLatin1("Radio button clicked is unknown: ") + buttonName);
		}
	}
	else
	{
		addError(QString::fromLatin1("Object clicked is not a quick item! Name: ") + objButton->objectName());
	}
}
