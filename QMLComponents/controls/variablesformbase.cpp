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
// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

#include "variablesformbase.h"
#include "variableslistbase.h"
#include "jasptheme.h"
#include "analysisform.h"
#include "analysisbase.h"
#include "workspace.h"
#include "log.h"

VariablesFormBase::VariablesFormBase(QQuickItem* parent) : JASPControl(parent), _hostSelection(this)
{
	_controlType			= ControlType::VariablesForm;
	_useControlMouseArea	= false;

	//The analysis (and with it the dataset selection) can only become known after construction,
	//AnalysisForm hands it out later on; formIsKnown tells us when that happened.
	connect(this, &JASPControl::formIsKnown, this, &VariablesFormBase::handleFormIsKnown);

	//The per-form selection state lives in _hostSelection, the Q_PROPERTYs mirror it:
	connect(&_hostSelection, &HostFilterSelection::selectedFilterIdChanged,		this, &VariablesFormBase::selectedFilterIdChanged);
	connect(&_hostSelection, &HostFilterSelection::selectionAvailableChanged,	this, &VariablesFormBase::selectionAvailableChanged);
	connect(&_hostSelection, &HostFilterSelection::filterSelectionValuesChanged, this, &VariablesFormBase::filterSelectionValuesChanged);
}

void VariablesFormBase::setUp()
{
	JASPControl::setUp();

	//AnalysisForm::setAnalysisUp() -> _setUp() is the hook static controls get (formIsKnown is only
	//emitted for dynamically created ones, see JASPControl's constructor), and by then the analysis
	//is attached. Reuse handleFormIsKnown: its connections are UniqueConnections, so getting here a
	//second time (dynamic controls also emit formIsKnown) is harmless.
	handleFormIsKnown(form());
}

void VariablesFormBase::handleFormIsKnown(AnalysisForm * form)
{
	if(!form)
		return;

	//AnalysisForm::setAnalysisUp() re-runs whenever the analysis or its filter changes; keep the
	//per-form selection in sync (it follows the analysis filter only until the user picks one).
	connect(form, &AnalysisForm::analysisChanged,	this, &VariablesFormBase::handleAnalysisChanged,			Qt::UniqueConnection);
	connect(form, &AnalysisForm::filterChanged,		this, &VariablesFormBase::handleAnalysisFilterChanged,	Qt::UniqueConnection);

	//The analysis is usually already attached when we get here - analysisChanged may well have fired
	//before this connection existed. Pull the current analysis in explicitly, otherwise the
	//selection never gets its default and QML keeps stale values.
	handleAnalysisChanged();
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
				if (variablesList->listViewType() == JASPControl::ListViewType::AvailableVariables)
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
			_controlsWidthSetByForm.push_back(control);
	}

	emit availableVariablesListChanged();
	emit allAssignedVariablesListChanged();
	emit allJASPControlsChanged();

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

void VariablesFormBase::handleAnalysisChanged()
{
	//With the analysis known, the selection (if this form declares one) marks the analysis
	//multiDataSetAware, hooks the workspace removal signals and gets its default: the analysis'
	//current filter. All QML bindings of the selection widget hang off these signals:
	_hostSelection.analysisBecameKnown();

	emit selectedFilterIdChanged();
	emit filterSelectionValuesChanged();
	emit selectionAvailableChanged();
}

AnalysisBase * VariablesFormBase::_owningAnalysis() const
{
	return form() ? form()->analysisObj() : nullptr;
}
