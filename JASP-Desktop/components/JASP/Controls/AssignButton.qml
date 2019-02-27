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
import JASP.Theme	1.0

Button
{
	id:				button
	hasTabFocus:	false

				property var	leftSource;
				property var	rightSource;
				property bool	leftToRight:	true

				property var	source:			leftToRight ? leftSource : rightSource;
				property var	target:			leftToRight ? rightSource : leftSource;

	readonly	property string iconToLeft:		"qrc:/images/arrow-left.png"
	readonly	property string iconToRight:	"qrc:/images/arrow-right.png"
	text:			""

	image.source:	leftToRight ? iconToRight : iconToLeft
    
	control.width:	40 * preferencesModel.uiScale
	control.height: 20 * preferencesModel.uiScale
	width:			control.width
	height:			control.height
	implicitWidth:  width
    implicitHeight:	height	
    
	onClicked:		source.moveSelectedItems(target)

	function setIconToRight()	{ if (leftSource.activeFocus)	 leftToRight = true;	}
	function setIconToLeft()	{ if (rightSource.activeFocus) leftToRight = false;		}
	function setDisabledState() { state = source.hasSelectedItems ? "" : "disabled"		}

	onSourceChanged:	setDisabledState()

    Component.onCompleted: {
        state = "disabled";
    }

}
