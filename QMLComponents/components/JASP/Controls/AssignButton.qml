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


import QtQuick		2.11

Button
{
	id:					button

				property var	leftSource
				property var	rightSource
				property bool	leftToRight:	true

				property var	sourceM:		leftToRight ? leftSource : rightSource
				property var	targetM:		leftToRight ? rightSource : leftSource

	readonly	property string iconToLeft:		jaspTheme.iconPath + "arrow-left.png"
	readonly	property string iconToRight:	jaspTheme.iconPath + "arrow-right.png"

	Connections
	{
		target: sourceM && sourceM.model? sourceM.model : null
		function onSelectedItemsTypesChanged() { Qt.callLater(setState); }
	}

	Connections
	{
		target: targetM && targetM.model ? targetM.model : null
		function onSelectedItemsTypesChanged() { Qt.callLater(setState); }
	}
	
	text:			""
	enabled:		false
	visible:		sourceM.visible && targetM.visible

	iconSource:		leftToRight ? iconToRight : iconToLeft

	control.buttonPadding:	2
	control.implicitWidth:	40 * preferencesModel.uiScale
	control.implicitHeight: 20 * preferencesModel.uiScale

	onClicked:			sourceM.moveSelectedItems(targetM)

	function setIconToRight()	{ if (leftSource.activeFocus)	leftToRight = true; setState(); }
	function setIconToLeft()	{ if (rightSource.activeFocus)	leftToRight = false; setState(); }
	function setState()
	{
		enabled = sourceM.enabled && targetM.enabled && sourceM.model && sourceM.model.selectedItems().length > 0 && targetM.areTypesAllowed(sourceM.model.selectedItemsTypes());
	}


}
