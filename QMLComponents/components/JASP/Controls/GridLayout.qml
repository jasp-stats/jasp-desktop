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

import QtQuick
import QtQuick.Layouts as QT


QT.GridLayout
{
	id:						gridLayout
	rowSpacing:				jaspTheme.rowGridSpacing
	columnSpacing:			jaspTheme.columnGridSpacing
	columns:				2
	QT.Layout.alignment:	Qt.AlignTop | Qt.AlignLeft
	
	property int count				: children.length

	property int _initialColumns	: 2
	property bool _initialized		: false

	Component.onCompleted:
	{
		_initialized = true;
		_initialColumns = columns; // Do not bind it!
		_checkColumns()
	}

	onImplicitWidthChanged:
	{
		// Wait a little bit, in case width is not yet updated.
		checkFormOverflowTimer.restart()
	}

	Timer
	{
		id: checkFormOverflowTimer
		interval: 50
		onTriggered: _checkColumns()
	}

	function _checkColumns()
	{
		if (!_initialized || (width === 0)) return;

		if (width < (implicitWidth - 1) && gridLayout.columns >= 2)
		{
			if (columnSpacing > (jaspTheme.columnGridSpacing / 2) && ((implicitWidth - width) < (jaspTheme.columnGridSpacing / 2) * (gridLayout.columns - 1)))
			{
				var newMargin = jaspTheme.columnGridSpacing - (implicitWidth - width) / (gridLayout.columns - 1) - 1
				messages.log("Content of the GridLayout is too large (width: " + width + ", implicitWidth: " + implicitWidth + "): decrease the margin between the columns from " + columnSpacing + " to " + newMargin)
				columnSpacing = newMargin
			}
			else
			{
				messages.log("Content of the GridLayout is too large (width: " + width + ", implicitWidth: " + implicitWidth + "): decrease the number of columns to " + (gridLayout.columns - 1))
				gridLayout.columns--;
			}
		}
	}

	onCountChanged:
	{
		for (var i = 0; i < children.length; i++)
		{
			if (typeof children[i].alignment !== "undefined")
				children[i].QT.Layout.alignment = children[i].alignment;
		}
	}
}
