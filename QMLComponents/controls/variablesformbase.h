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

class VariablesListBase;

class VariablesFormBase : public JASPControl
{
	Q_OBJECT

	Q_PROPERTY( JASPControl*			availableVariablesList			READ availableVariablesList													NOTIFY availableVariablesListChanged		)
	Q_PROPERTY( QList<JASPControl*>		allAssignedVariablesList		READ allAssignedVariablesList												NOTIFY allAssignedVariablesListChanged		)
	Q_PROPERTY( QList<JASPControl*>		allJASPControls					READ allJASPControls														NOTIFY allJASPControlsChanged				)
	Q_PROPERTY( qreal					marginBetweenVariablesLists		READ marginBetweenVariablesLists	WRITE setMarginBetweenVariablesLists	NOTIFY marginBetweenVariablesListsChanged	)
	Q_PROPERTY( qreal					minimumHeightVariablesLists		READ minimumHeightVariablesLists	WRITE setMinimumHeightVariablesLists	NOTIFY minimumHeightVariablesListsChanged	)

public:
	VariablesFormBase(QQuickItem* parent = nullptr);

	JASPControl*			availableVariablesList()		const;
	QList<JASPControl*>		allAssignedVariablesList()		const	{ return _allAssignedVariablesList;		}
	QList<JASPControl*>		allJASPControls()				const	{ return _allJASPControls;				}
	qreal					marginBetweenVariablesLists()	const	{ return _marginBetweenVariablesLists;	}
	qreal					minimumHeightVariablesLists()	const	{ return _minimumHeightVariablesLists;	}

	Q_INVOKABLE bool		widthSetByForm(JASPControl* control)	{ return _controlsWidthSetByForm.contains(control); }
	Q_INVOKABLE bool		heightSetByForm(JASPControl* control)	{ return _controlsHeightSetByForm.contains(control); }

signals:
	void availableVariablesListChanged();
	void allAssignedVariablesListChanged();
	void allJASPControlsChanged();
	void marginBetweenVariablesListsChanged();
	void minimumHeightVariablesListsChanged();

protected:
	void componentComplete() override;

protected slots:
	void setMarginBetweenVariablesLists(qreal value);
	void setMinimumHeightVariablesLists(qreal value);
	void setControlsSizeSlot();

private:

	VariablesListBase*			_availableVariablesList = nullptr;
	QList<JASPControl*>			_allAssignedVariablesList,
								_allJASPControls,
								_controlsWidthSetByForm,
								_controlsHeightSetByForm;
	qreal						_marginBetweenVariablesLists = 8;
	qreal						_minimumHeightVariablesLists = 25;

};

#endif // VARIABLESFROMBASE_H
