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
import QtQuick.Controls
import JASP.Controls

/*!
    \qmltype Switch
    \inqmlmodule JASP.Controls 1.0
    \brief A toggle switch control.

	A sliding toggle indicator that can be used instead of a
    checkbox. Binds a boolean value to R, similar to CheckBox but with
    a different visual appearance.

    \section1 R Binding

    \list
    \li \b{R Type:} \c logical
    \li \b{Default:} false
    \endlist

    \section1 Properties

    \list
    \li \b name (string) - R option name this control binds to. Default: "".
    \li \b label (string) - Text displayed next to the switch. Default: "".
    \li \b checked (bool) - Whether the switch is on. Default: false.
    \endlist

    \section1 Inherited Properties

    \list
    \li \b enabled (bool) - Whether the control is interactive. Default: true.
    \li \b visible (bool) - Whether the control is visible. Default: true.
    \li \b info (string) - Info that will be used by tooltip and to generate the help. Default: "".
    \li \b toolTip (string) - This property overwrite info property, in order to display a simpler tooltip text. Default: "".
    \endlist

    \section1 Example

    \qml
    Switch {
        name: "darkMode"
        label: qsTr("Dark mode")
    }
    \endqml
*/
CheckBoxBase
{
	implicitWidth:				control.indicator.height + (4 * preferencesModel.uiScale)
	implicitHeight:				control.indicator.width + controlLabel.implicitWidth + control.spacing + (6 * preferencesModel.uiScale)
	innerControl:				control
	title:						label
	
	property alias control:		control
	property alias label:		control.text
	property alias checked:		control.checked
    signal clicked();
    
	Component.onCompleted: control.clicked.connect(clicked);
    
	Switch
	{
		id:				control
		anchors.fill:	parent

		indicator:	Rectangle
		{
			id:				switchHandle
			width:			jaspTheme.switchHeight * 2.2
			height:			jaspTheme.switchHeight
			radius:			jaspTheme.switchHeight / 2
			color:			jaspTheme.light
			border.color:	jaspTheme.borderColor
			anchors
			{
				left:		control.left
				leftMargin: 2 * preferencesModel.uiScale
				top:		control.top
				topMargin:	2 * preferencesModel.uiScale
			}
    
			Rectangle
			{
				id:				rectangle
				width:			jaspTheme.switchHeight
				height:			jaspTheme.switchHeight
				radius:			jaspTheme.switchHeight / 2
				color:			jaspTheme.light
				border.color:	jaspTheme.borderColor
            }
        }
        
		contentItem: Label
		{
			id:				controlLabel
			text:			control.text
			font:			jaspTheme.font
			color:			enabled ? jaspTheme.textEnabled : jaspTheme.textDisabled
			anchors
			{
				left:		control.indicator.right
				leftMargin: control.spacing
				top:		control.top
				topMargin:	2 * preferencesModel.uiScale
			}
        }
    }
}
