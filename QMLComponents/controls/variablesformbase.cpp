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
#include "jasptheme.h"
#include "analysisform.h"
#include "analysisbase.h"
#include "workspace.h"
#include "log.h"

VariablesFormBase::VariablesFormBase(QQuickItem* parent) : JASPControl(parent)
{
	_controlType			= ControlType::VariablesForm;
	_useControlMouseArea	= false;

	//The analysis (and with it whether dataSetSelection is allowed) can only become known after
	//construction, AnalysisForm hands it out later on; formIsKnown tells us when that happened.
	connect(this, &JASPControl::formIsKnown, this, &VariablesFormBase::handleFormIsKnown);
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

	if(Workspace::singleton())
	{
		connect(Workspace::singleton(), &Workspace::dataSetFilterDropDownListChanged, this, &VariablesFormBase::dataSetSelectionValuesChanged, Qt::UniqueConnection);
		connect(Workspace::singleton(), &Workspace::dataSetCreated,					this, &VariablesFormBase::dataSetTitleValuesChanged,		Qt::UniqueConnection);
		connect(Workspace::singleton(), &Workspace::dataSetRemoved,					this, &VariablesFormBase::dataSetTitleValuesChanged,		Qt::UniqueConnection);
		connect(Workspace::singleton(), &Workspace::dataSetTitleChanged,				this, &VariablesFormBase::dataSetTitleValuesChanged,		Qt::UniqueConnection);
	}

	//AnalysisForm::setAnalysisUp() re-runs whenever the analysis or its filter changes; keep the
	//selection properties in sync with whatever the analysis is doing elsewhere (filter button, RPC, ...).
	connect(form, &AnalysisForm::analysisChanged,				this, &VariablesFormBase::handleAnalysisChanged,		Qt::UniqueConnection);
	connect(form, &AnalysisForm::filterChanged,					this, &VariablesFormBase::handleAnalysisFilterChanged,	Qt::UniqueConnection);

	//The analysis is usually already attached when we get here - analysisChanged may well have fired
	//before this connection existed. Pull the current analysis in explicitly, otherwise the
	//multiDataSetAwareChanged hookup inside handleAnalysisChanged never happens and QML keeps a stale
	//dataSetSelectionAllowed=false (the getter is right, only the binding never re-evaluated).
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
	//The awareness of the analysis drives dataSetSelectionAllowed, so follow its changes too (the
	//QML bindings of the selection widget lean on that signal to stay up-to-date).
	if(AnalysisBase * analysis = _owningAnalysis())
	{
		connect(analysis, &AnalysisBase::multiDataSetAwareChanged, this, &VariablesFormBase::handleAnalysisChanged, Qt::UniqueConnection);

		//A dataSetSelectionOption makes this a multi-dataset analysis by definition. In desktop the
		//AnalysisEntry pushes that flag onto the analysis, but the syntax bridge runs on a dummy
		//AnalysisBase that gets nothing - so the form declares it here, as soon as both are known
		//(before any option value binds and .meta gets stamped).
		if(!_dataSetSelectionOption.isEmpty() && !analysis->multiDataSetAware())
			analysis->setMultiDataSetAware(true);
	}

	emit dataSetSelectionAllowedChanged();
	emit selectedFilterIdChanged();
	emit dataSetSelectionValuesChanged();
	emit dataSetTitleValuesChanged();
}

void VariablesFormBase::handleAnalysisFilterChanged()
{
	emit selectedFilterIdChanged();
}

AnalysisBase * VariablesFormBase::_owningAnalysis() const
{
	return form() ? form()->analysisObj() : nullptr;
}

QVariantList VariablesFormBase::dataSetSelectionValues() const
{
	Workspace * workspace = Workspace::singleton();

	return workspace ? workspace->dataSetFilterDropDownList() : QVariantList();
}

bool VariablesFormBase::dataSetSelectionAllowed() const
{
	AnalysisBase * analysis = _owningAnalysis();

	//Inert (but not an error) for analyses that are not multiDataSetAware, like the dummy analysis of
	//R-syntax mode; only a real aware analysis lets the user select another dataset/filter here.
	return analysis ? analysis->multiDataSetAware() : false;
}

int VariablesFormBase::selectedFilterId() const
{
	AnalysisBase * analysis = _owningAnalysis();

	return analysis ? analysis->filterId() : -1;
}

void VariablesFormBase::setSelectedFilterId(int filterId)
{
	AnalysisBase * analysis = _owningAnalysis();

	if(!analysis || !dataSetSelectionAllowed() || analysis->filterId() == filterId)
		return;

	//Selecting a dataset is selecting one of its filters: hand it to the analysis and everything
	//follows - the form's VariableInfo provider, the revalidation of all lists and the rerun.
	analysis->setFilterId(filterId);
}

void VariablesFormBase::setDataSetSelection(bool dataSetSelection)
{
	if (_dataSetSelection == dataSetSelection)
		return;

	_dataSetSelection = dataSetSelection;

	if(_dataSetSelection)
	{
		if(Workspace::singleton())
			connect(Workspace::singleton(), &Workspace::dataSetFilterDropDownListChanged, this, &VariablesFormBase::dataSetSelectionValuesChanged, Qt::UniqueConnection);

		emit dataSetSelectionValuesChanged();
		emit dataSetSelectionAllowedChanged();
	}

	emit dataSetSelectionChanged();
}

void VariablesFormBase::setDataSetSelectionOption(const QString & option)
{
	if (_dataSetSelectionOption == option)
		return;

	_dataSetSelectionOption = option;

	//An option-driven dataset selection implies a multi-dataset analysis; in the bridge the dummy
	//analysis gets no flag from anywhere else (handleAnalysisChanged covers the case where the
	//analysis only shows up after this property was bound).
	if(!_dataSetSelectionOption.isEmpty() && _owningAnalysis() && !_owningAnalysis()->multiDataSetAware())
		_owningAnalysis()->setMultiDataSetAware(true);

	emit dataSetSelectionOptionChanged();
}

QVariantList VariablesFormBase::dataSetTitleValues() const
{
	Workspace    * workspace = Workspace::singleton();
	QVariantList   values;

	if (workspace)
		for (DataSet * dataSet : workspace->dataSets())
			if (dataSet)
				values.append(QVariantMap({ {"value", dataSet->title()}, { "label", dataSet->title() } }));

	return values;
}

void VariablesFormBase::selectDataSetByName(const QString & name)
{
	Workspace    * workspace = Workspace::singleton();
	AnalysisBase * analysis  = _owningAnalysis();

	QStringList available;

	if (workspace)
		for (DataSet * dataSet : workspace->dataSets())
			if (dataSet)
				available << dataSet->title();

	if (name.trimmed().isEmpty())
	{
		addControlError(tr("The dataset selection option '%1' must not be empty (datasets loaded: %2)")
						.arg(dataSetSelectionOption(), available.join(", ")));
		return;
	}

	DataSet * dataSet = workspace ? workspace->dataSetByTitle(name) : nullptr;

	if (!dataSet)
	{
		addControlError(tr("The dataset selection option '%1' names '%2', but no such dataset is loaded (datasets loaded: %3)")
						.arg(dataSetSelectionOption(), name, available.join(", ")));
		return;
	}

	_selectedDataSetTitle = name;
	applyDataSetSelection();
}

void VariablesFormBase::applyDataSetSelection()
{
	if (_dataSetSelectionOption.isEmpty() || _selectedDataSetTitle.isEmpty())
		return;

	Workspace    * workspace = Workspace::singleton();
	AnalysisBase * analysis  = _owningAnalysis();
	DataSet      * dataSet   = workspace ? workspace->dataSetByTitle(_selectedDataSetTitle) : nullptr;

	if (!dataSet || !analysis || !dataSet->defaultFilter())
		return;

	//Syntax mode has no user filters (the data arrives prefiltered), so selecting a dataset means
	//selecting its default filter: the form's variable info, the .meta stamping of the values bound
	//afterwards (BoundControlBase::createMeta) and the per-dataset encoding all follow the analysis.
	if (analysis->filterId() != dataSet->defaultFilter()->id())
		analysis->setFilterId(dataSet->defaultFilter()->id());

	if (workspace->shownDataSet() != dataSet)
		workspace->setShownDataSet(dataSet);

	emit selectedFilterIdChanged();
}
