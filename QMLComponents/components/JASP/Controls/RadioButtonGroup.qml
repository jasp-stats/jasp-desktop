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
import QtQuick.Layouts	as L
import JASP.Controls


/*!
    \qmltype RadioButtonGroup
    \inqmlmodule JASP.Controls 1.0
    \brief A group of mutually exclusive radio button options.

	Contains RadioButton children, of which
    exactly one can be checked at a time. The checked button's name is sent
    as the option value to R.

    \section1 R Binding

    \list
    \li \b{R Type:} \c character (the name/value of the selected RadioButton)
    \li \b{Default:} Value of the initially checked RadioButton
    \endlist

    \section1 Properties

    \list
    \li \b name (string) - R option name this control binds to. Default: "".
    \li \b title (string) - Title label displayed above or beside the group. Alias: text. Default: "".
    \li \b radioButtonsOnSameRow (bool) - Place all radio buttons on one row. Default: false.
    \li \b columns (int) - Number of columns in the content area. Default: 1 (or children.length when radioButtonsOnSameRow).
	\li \b leftPadding (int) - Left padding for the content area. Default: 10.
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
    RadioButtonGroup {
        name: "hypothesis"
        title: qsTr("Alt. Hypothesis")
        RadioButton { value: "twoSided"; label: qsTr("≠ Test value"); checked: true }
        RadioButton { value: "greater";  label: qsTr("> Test value") }
        RadioButton { value: "less";     label: qsTr("< Test value") }
    }
    \endqml
*/
RadioButtonsGroupBase
{
	id:					control
	childControlsArea:	contentArea
	focusOnTab:			false
	shouldStealHover:	false
	mouseAreaZone:		title !== "" ? label : control

	default property alias	content:				contentArea.children
			property bool	radioButtonsOnSameRow:	false
			property alias	columns:				contentArea.columns
			property alias	text:					control.title
			property int	leftPadding:			jaspTheme.groupContentPadding
			property int	textFormat:				Text.AutoText

	implicitWidth:	radioButtonsOnSameRow
						? contentArea.x + contentArea.implicitWidth
						: Math.max(label.implicitWidth, contentArea.x + contentArea.implicitWidth)

	implicitHeight:	radioButtonsOnSameRow
						? Math.max(label.implicitHeight, contentArea.implicitHeight)
						: contentArea.y + contentArea.implicitHeight	
    
	L.Layout.leftMargin:	indent ? jaspTheme.indentationLength : 0
	
	Label
	{
		id:				label
		text:			control.title
		visible:		control.title && control.visible ? true : false
		anchors.top:	control.top
		anchors.left:	control.left
		anchors.bottom:	radioButtonsOnSameRow ? contentArea.bottom : undefined
		verticalAlignment: Text.AlignVCenter
		font:			jaspTheme.font
		color:			enabled ? jaspTheme.textEnabled : jaspTheme.textDisabled		
		textFormat:		control.textFormat
    }
    

	GridLayout
	{
		id:					contentArea
		rowSpacing:			jaspTheme.rowGroupSpacing
		columnSpacing:		jaspTheme.columnGroupSpacing
		columns:			radioButtonsOnSameRow ? children.length : 1
		anchors.top:		control.title && !radioButtonsOnSameRow ? label.bottom : control.top
		anchors.topMargin:	control.title && !radioButtonsOnSameRow ? jaspTheme.titleBottomMargin : 0
		anchors.left:		control.title && radioButtonsOnSameRow ? label.right : control.left
		anchors.leftMargin: control.title ? jaspTheme.groupContentPadding : 0
	}

	background: backgroundBox

	Rectangle
	{
		// This rectangle is only here to show the dependency outline for "Show Dependencies"
		id:					backgroundBox
		color:				"transparent"
		border.width:		0
		anchors.fill:		parent
		anchors.margins:	-1 * border.width
		z:					-1
		visible:			preferencesModel.developerMode
	}

	Component.onDestruction:
	{
		unregisterAll();
	}
}
