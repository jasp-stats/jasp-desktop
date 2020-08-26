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

import QtQuick 2.11
import QtQuick.Layouts 1.3


GridLayout
{
	id:						gridLayout
	rowSpacing:				jaspTheme.rowGridSpacing
	columnSpacing:			jaspTheme.columnGridSpacing
	columns:				2
	Layout.minimumWidth:	parent.width
	Layout.alignment:		Qt.AlignTop | Qt.AlignLeft
	
	property int count									: children.length
	property bool checkFormOverflowWhenLanguageChanged	: true

	property int _initialColumns	: 2

	Component.onCompleted: _initialColumns = columns; // Do not bind it!

	Connections
	{
		enabled:					checkFormOverflowWhenLanguageChanged
		target:						preferencesModel
		onLanguageCodeChanged:		checkFormOverflowTimer.restart()
		onRealInterfaceFontChanged:	checkFormOverflowTimer.restart()
	}

	Timer
	{
		id: checkFormOverflowTimer
		interval: 50
		onTriggered: checkFormOverflow()
	}


	function checkFormOverflow()
	{
		if (!form) return false;

		var startColumns = gridLayout.columns;

		if (gridLayout.columns !== gridLayout._initialColumns)
			gridLayout.columns = gridLayout._initialColumns;

		var decrementColumns = true;

		while (decrementColumns && gridLayout.columns >= 2)
		{
			decrementColumns = false;
			for (var i = 0; i < gridLayout.children.length; i++)
			{
				var child = gridLayout.children[i];
				if (child.mapToItem(form, child.width, 0).x > form.width)
					decrementColumns = true;
			}

			if (decrementColumns)
				gridLayout.columns--;
		}

		return startColumns !== gridLayout.columns;
	}
}
