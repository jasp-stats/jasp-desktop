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

#ifndef VARIABLESFROMBASE_H
#define VARIABLESFROMBASE_H

#include "jaspcontrol.h"
#include <QVariantList>

class VariablesListBase;
class AnalysisBase;

class VariablesFormBase : public JASPControl
{
	Q_OBJECT
	QML_ELEMENT

	Q_PROPERTY( JASPControl*			availableVariablesList			READ availableVariablesList													NOTIFY availableVariablesListChanged		)
	Q_PROPERTY( QList<JASPControl*>		allAssignedVariablesList		READ allAssignedVariablesList												NOTIFY allAssignedVariablesListChanged		)
	Q_PROPERTY( QList<JASPControl*>		allJASPControls					READ allJASPControls														NOTIFY allJASPControlsChanged				)
	Q_PROPERTY( qreal					marginBetweenVariablesLists		READ marginBetweenVariablesLists	WRITE setMarginBetweenVariablesLists	NOTIFY marginBetweenVariablesListsChanged	)
	Q_PROPERTY( qreal					minimumHeightVariablesLists		READ minimumHeightVariablesLists	WRITE setMinimumHeightVariablesLists	NOTIFY minimumHeightVariablesListsChanged	)
	/// Enabling this puts a dataset/filter selection on the form: every filter of every dataset in the
	/// workspace can be picked and the whole form follows it (selecting a dataset always means selecting
	/// one of its filters; see AnalysisBase::setFilterId). Only meaningful for multiDataSetAware analyses,
	/// so it is silently inert for the others (see dataSetSelectionAllowed).
	Q_PROPERTY( bool					dataSetSelection				READ dataSetSelection			WRITE setDataSetSelection			NOTIFY dataSetSelectionChanged			)
	///< Entries {value: "<filterId>", label: "DataSet - Filter"}, the workspace's dataSetFilterDropDownList.
	Q_PROPERTY( QVariantList			dataSetSelectionValues			READ dataSetSelectionValues													NOTIFY dataSetSelectionValuesChanged		)
	///< False while the bound analysis is known not to be multiDataSetAware.
	Q_PROPERTY( bool					dataSetSelectionAllowed			READ dataSetSelectionAllowed												NOTIFY dataSetSelectionAllowedChanged		)
	///< The filter this form currently runs on (the analysis' filterId), -1 when unknown; setting it
	///< switches the whole form (and thus the analysis) to that dataset/filter.
	Q_PROPERTY( int						selectedFilterId				READ selectedFilterId			WRITE setSelectedFilterId			NOTIFY selectedFilterIdChanged		)
	///< Syntax mode only: name of the OPTION that carries this form's dataset selection (a dataset
	///< title; see selectDataSetByName). Setting it also marks the analysis multiDataSetAware (there
	///< is no AnalysisEntry in the bridge to push that flag). Empty (the default) means: no
	///< option-driven selection, desktop keeps using the dataSetSelection dropdown above.
	Q_PROPERTY( QString					dataSetSelectionOption			READ dataSetSelectionOption		WRITE setDataSetSelectionOption	NOTIFY dataSetSelectionOptionChanged	)
	///< {value: title, label: title} per workspace dataset; the values a dataSetSelectionOption may hold.
	Q_PROPERTY( QVariantList			dataSetTitleValues				READ dataSetTitleValues			NOTIFY dataSetTitleValuesChanged		)

public:
	VariablesFormBase(QQuickItem* parent = nullptr);

	JASPControl*			availableVariablesList()		const;
	QList<JASPControl*>		allAssignedVariablesList()		const	{ return _allAssignedVariablesList;		}
	QList<JASPControl*>		allJASPControls()				const	{ return _allJASPControls;				}
	qreal					marginBetweenVariablesLists()	const	{ return _marginBetweenVariablesLists;	}
	qreal					minimumHeightVariablesLists()	const	{ return _minimumHeightVariablesLists;	}
	bool					dataSetSelection()			const	{ return _dataSetSelection;					}
	QVariantList			dataSetSelectionValues()		const;
	bool					dataSetSelectionAllowed()		const;
	int						selectedFilterId()			const;
	QString					dataSetSelectionOption()		const	{ return _dataSetSelectionOption;			}
	///< {value: title, label: title} per workspace dataset; the values a dataSetSelectionOption may hold.
	QVariantList			dataSetTitleValues()			const;

	///< Switches this form (and analysis) to the dataset with exactly this title: the syntax-mode
	///< counterpart of the desktop dataSetSelection dropdown (users pre-filter their data, so the
	///< dataset's default filter is selected). Empty or unknown names raise a control error.
	Q_INVOKABLE void		selectDataSetByName(const QString & name);

	///< Re-selects the dataset chosen through dataSetSelectionOption (if any) on the owning
	///< analysis: JASPControl calls this right before binding any of its children, so a control
	///  validates and stamps against ITS OWN form's dataset regardless of the order in which the
	///  form applies the options (the analysis filter is one global slot; each form owns it for a
	///  binding moment). No-op for desktop (no dataSetSelectionOption there).
	void					applyDataSetSelection();

	Q_INVOKABLE bool		widthSetByForm(JASPControl* control)	{ return _controlsWidthSetByForm.contains(control); }
	Q_INVOKABLE bool		heightSetByForm(JASPControl* control)	{ return _controlsHeightSetByForm.contains(control); }

public slots:
	void					setMarginBetweenVariablesLists(qreal value);
	void					setMinimumHeightVariablesLists(qreal value);
	void					setDataSetSelection(bool dataSetSelection);
	void					setSelectedFilterId(int filterId);
	void					setDataSetSelectionOption(const QString & option);

signals:
	void availableVariablesListChanged();
	void allAssignedVariablesListChanged();
	void allJASPControlsChanged();
	void marginBetweenVariablesListsChanged();
	void minimumHeightVariablesListsChanged();
	void dataSetSelectionChanged();
	void dataSetSelectionValuesChanged();
	void dataSetSelectionAllowedChanged();
	void selectedFilterIdChanged();
	void dataSetSelectionOptionChanged();
	void dataSetTitleValuesChanged();

protected:
	void componentComplete() override;
	///< Static controls never receive formIsKnown (only dynamically created ones emit it), so this
	///< is where a VariablesForm sitting in a normal form hooks itself up to its AnalysisForm.
	void setUp() override;

private slots:
	void					handleFormIsKnown(AnalysisForm * form);
	void					handleAnalysisChanged();
	void					handleAnalysisFilterChanged();

private:
	AnalysisBase	*		_owningAnalysis()			const;

	VariablesListBase*			_availableVariablesList = nullptr;
	QList<JASPControl*>			_allAssignedVariablesList,
								_allJASPControls,
								_controlsWidthSetByForm,
								_controlsHeightSetByForm;
	qreal						_marginBetweenVariablesLists = 8;
	qreal						_minimumHeightVariablesLists = 25;
	bool						_dataSetSelection = false;
	QString						_dataSetSelectionOption;
	QString						_selectedDataSetTitle;	///< what dataSetSelectionOption last resolved to

};

#endif // VARIABLESFROMBASE_H
