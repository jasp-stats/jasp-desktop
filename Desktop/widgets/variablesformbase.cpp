//
// Copyright (C) 2013-2021 University of Amsterdam
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

#include "variablesformbase.h"
#include "variableslistbase.h"
#include "qquick/jasptheme.h"

VariablesFormBase::VariablesFormBase(QQuickItem* parent) : JASPControl(parent)
{
	_controlType = ControlType::VariablesForm;
}

void VariablesFormBase::componentComplete()
{
	JASPControl::componentComplete();

	_allJASPControls.clear();
	_allAssignedVariablesList.clear();
	_availableVariablesList = nullptr;

	QQuickItem* contentItems = property("contentItems").value<QQuickItem*>();
	QList<QQuickItem*> items = contentItems->childItems();

	bool debugMode = false;
#ifdef JASP_DEBUG
	debugMode = true;
#endif

	for (QQuickItem* item : items)
	{
		JASPControl* control = qobject_cast<JASPControl*>(item);
		if (!control) continue;

		if (debug())	control->setDebug(true);
		if (debugMode || !control->debug())
		{
			VariablesListBase* variablesList = qobject_cast<VariablesListBase*>(control);
			if (variablesList)
			{
				if (variablesList->listViewType() == JASPControl::ListViewType::AvailableVariables || variablesList->listViewType() == JASPControl::ListViewType::AvailableInteraction)
				{
					if (_availableVariablesList)
						addControlError(tr("Only 1 Available Variables list can be set in a VariablesForm"));

					_availableVariablesList = variablesList;
				}
				else
					_allAssignedVariablesList.push_back(control);
			}
			if (control != _availableVariablesList)
				_allJASPControls.push_back(control);

			control->setParentItem(this);
		}
	}

	if (!_availableVariablesList)
	{
		addControlError(tr("There is no Available List in the Variables Form"));
		return;
	}

	_availableVariablesList->setY(0);
	_availableVariablesList->setX(0);

	// Set the width of the VariablesList to listWidth only if it is not set explicitely
	// Implicitely, the width is set to the parent width.
	if (qFuzzyCompare(_availableVariablesList->width(), width()))
		_controlsWidthSetByForm.push_back(_availableVariablesList);

	for (JASPControl* control : _allJASPControls)
	{
		ControlType type = control->controlType();
		if ((type == ControlType::VariablesListView) || (type == ControlType::FactorLevelList) || (type == ControlType::InputListView))
		{
			if (qFuzzyCompare(control->width(), width()))
				_controlsWidthSetByForm.push_back(control);

			if (qFuzzyCompare(control->height(), double(JaspTheme::currentTheme()->defaultVariablesFormHeight())))
				_controlsHeightSetByForm.push_back(control);
		}
		else if (type == ControlType::ComboBox)
		{
			_controlsWidthSetByForm.push_back(control);
			connect(control, &QQuickItem::heightChanged, this, &VariablesFormBase::setControlsSizeSlot);
		}
	}

	QMetaObject::invokeMethod(this, "init");

	setInitialized();
}

void VariablesFormBase::setMarginBetweenVariablesLists(qreal value)
{
	if (qFuzzyCompare(value, _marginBetweenVariablesLists))
	{
		_marginBetweenVariablesLists = value;
		emit marginBetweenVariablesListsChanged();
	}
}

void VariablesFormBase::setMinimumHeightVariablesLists(qreal value)
{
	if (qFuzzyCompare(value, _minimumHeightVariablesLists))
	{
		_minimumHeightVariablesLists = value;
		emit minimumHeightVariablesListsChanged();
	}
}

JASPControl* VariablesFormBase::availableVariablesList() const
{
	return _availableVariablesList;
}

void VariablesFormBase::setControlsSizeSlot()
{
	QMetaObject::invokeMethod(this, "setControlsSize");
}
