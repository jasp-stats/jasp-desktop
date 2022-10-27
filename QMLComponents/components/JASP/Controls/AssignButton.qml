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

				property var	source:			leftToRight ? leftSource : rightSource
				property var	target:			leftToRight ? rightSource : leftSource

	readonly	property string iconToLeft:		jaspTheme.iconPath + "arrow-left.png"
	readonly	property string iconToRight:	jaspTheme.iconPath + "arrow-right.png"
	
	text:			""
	enabled:		false
	visible:		source.visible && target.visible

	iconSource:		leftToRight ? iconToRight : iconToLeft

	control.buttonPadding:	2
	control.implicitWidth:	40 * preferencesModel.uiScale
	control.implicitHeight: 20 * preferencesModel.uiScale

	onClicked:			source.moveSelectedItems(target)

	function setIconToRight()	{ if (leftSource.activeFocus)	leftToRight = true; setState(); }
	function setIconToLeft()	{ if (rightSource.activeFocus)	leftToRight = false; setState(); }
	function setState()
	{
		var isEnabled = source.enabled && target.enabled && source.model && source.model.selectedItems().length > 0;
		if (isEnabled)
		{
			if (target.allowedColumns.length > 0)
			{
				isEnabled = false;
				var sourceSelectedItemsTypes = source.model.selectedItemsTypes()
				for (var i = 0; i < sourceSelectedItemsTypes.length; i++)
				{
					var itemType = sourceSelectedItemsTypes[i];
					if (target.allowedColumns.includes(itemType))
						isEnabled = true;
				}
			}
		}
		
		enabled = isEnabled
	}


}
