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
#include "hostfilterselection.h"
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
	///< Name of the OPTION that carries this form's dataset/filter selection (value: a filter id);
	///< setting it gives the form a selection of its own: a FilterSelect dropdown on the form
	///  reads/writes the option (desktop and syntax alike) and every list below the form shows the
	///  columns of the selected filter - independent from any other form of the same analysis.
	///  It also marks the analysis multiDataSetAware (in the bridge nothing else sets that flag).
	Q_PROPERTY( QString					dataSetSelectionOption			READ dataSetSelectionOption		WRITE setDataSetSelectionOption	NOTIFY dataSetSelectionOptionChanged	)
	///< True when this form has a dataSetSelectionOption (so its FilterSelect should exist).
	Q_PROPERTY( bool					selectionAvailable				READ selectionAvailable													NOTIFY selectionAvailableChanged			)
	///< {value: "<filterId>", label: "DataSet - Filter"}: every filter of every workspace dataset.
	Q_PROPERTY( QVariantList			filterSelectionValues			READ filterSelectionValues												NOTIFY filterSelectionValuesChanged		)
	///< This form's own selection (never the analysis' filter!), -1 while none. Setting an unknown
	///< filter id is rejected. Assigning it is what the FilterSelect dropdown and the tests do.
	Q_PROPERTY( int						selectedFilterId				READ selectedFilterId			WRITE setSelectedFilterId			NOTIFY selectedFilterIdChanged			)

public:
	VariablesFormBase(QQuickItem* parent = nullptr);

	JASPControl*			availableVariablesList()		const;
	QList<JASPControl*>		allAssignedVariablesList()		const	{ return _allAssignedVariablesList;		}
	QList<JASPControl*>		allJASPControls()				const	{ return _allJASPControls;				}
	qreal					marginBetweenVariablesLists()	const	{ return _marginBetweenVariablesLists;	}
	qreal					minimumHeightVariablesLists()	const	{ return _minimumHeightVariablesLists;	}
	QString					dataSetSelectionOption()		const	{ return _hostSelection.option();			}
	bool					selectionAvailable()			const	{ return _hostSelection.selectionAvailable(); }
	QVariantList			filterSelectionValues()			const	{ return _hostSelection.filterSelectionValues(); }
	int						selectedFilterId()			const	{ return _hostSelection.selectedFilterId(); }

	///< This form's VariableInfo once it has a selection (provider = the selected filter) - what
	///< every consumer below the form resolves to through JASPControl::effectiveVarInfo().
	VariableInfo		  * ownedSelectionVarInfo() override				{ return _hostSelection.selectionAvailable() ? _hostSelection.ownVarInfo() : nullptr; }

	Q_INVOKABLE bool		widthSetByForm(JASPControl* control)	{ return _controlsWidthSetByForm.contains(control); }
	Q_INVOKABLE bool		heightSetByForm(JASPControl* control)	{ return _controlsHeightSetByForm.contains(control); }

public slots:
	void					setMarginBetweenVariablesLists(qreal value);
	void					setMinimumHeightVariablesLists(qreal value);
	void					setDataSetSelectionOption(const QString & option)	{ _hostSelection.setOption(option); emit dataSetSelectionOptionChanged(); }
	void					setSelectedFilterId(int filterId)					{ _hostSelection.setSelectedFilterId(filterId); }

signals:
	void availableVariablesListChanged();
	void allAssignedVariablesListChanged();
	void allJASPControlsChanged();
	void marginBetweenVariablesListsChanged();
	void minimumHeightVariablesListsChanged();
	void dataSetSelectionOptionChanged();
	void selectionAvailableChanged();
	void filterSelectionValuesChanged();
	void selectedFilterIdChanged();

protected:
	void componentComplete() override;
	///< Static controls never receive formIsKnown (only dynamically created ones emit it), so this
	///< is where a VariablesForm sitting in a normal form hooks itself up to its AnalysisForm.
	void setUp() override;

private slots:
	void					handleFormIsKnown(AnalysisForm * form);
	void					handleAnalysisChanged();
	void					handleAnalysisFilterChanged()	{ _hostSelection.analysisFilterMaybeChanged(); }

private:
	AnalysisBase	*		_owningAnalysis()			const;

	VariablesListBase*			_availableVariablesList = nullptr;
	QList<JASPControl*>			_allAssignedVariablesList,
								_allJASPControls,
								_controlsWidthSetByForm,
								_controlsHeightSetByForm;
	qreal						_marginBetweenVariablesLists = 8;
	qreal						_minimumHeightVariablesLists = 25;
	HostFilterSelection			_hostSelection;

};

#endif // VARIABLESFROMBASE_H
