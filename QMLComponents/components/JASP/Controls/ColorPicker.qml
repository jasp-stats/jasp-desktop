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
import JASP.Controls
import QtQuick.Dialogs

/*!
	\qmltype ColorPicker
	\inqmlmodule JASP.Controls 1.0
	\brief ColorPicker allows the user to select a color.

	This control allows the user to select a color, either by typing directly the name or the hexadecimal value of the color, or by picking a color from a color palette, or even by picking a color somewhere on your screen.
	The control is composed by a textField and a button:
	\list
	\li \b TextField - displays the current color name (or hexadecomal value),
	\li \b Button - its color dispayed the current color value, and by clicking on it, a ColorPicker pops up, where the user can choose a color by picking a color from different kinds of palettes, or by clicking on a color somewhere on your screen.
	\endlist

	\section1 R Binding

	\list
	\li \b{R Type:} \c string
	\li \b{Default:} JASP blue color
	\endlist

	\section1 Properties

	\list
	\li \b buttonText (string) - Text displayed on . Default: "Click".
	\li \b buttonInfo (string) - Label displayed above the list. Alias: label. Default: "".
	\li \b setButtonOnder (boolean) - Boolean that sets the button onder or beside the TextField. Default false
	\li \b textField (var) - Alias that accesses the textField item.
	\li \b button (var) - Alias that accesses the Button item.
	\endlist

	\section1 Inherited Properties from TextField

	\list
	\li \b name (string) - R option name this control binds to. Default: "".
	\li \b value (string) - Current color value. Default: the JASP blue color.
	\li \b defaultValue (var) - Default color value. Default: the JASP blue color.
	\li \b label (string) - Label displayed before the field. Default: "".
	\endlist

	\section1 Other Inherited Properties

	\list
	\li \b enabled (bool) - Whether the control is interactive. Default: true.
	\li \b visible (bool) - Whether the control is visible. Default: true.
	\li \b info (string) - Info that will be used by tooltip and to generate the help. Default: "".
	\li \b toolTip (string) - This property overwrite info property, in order to display a simpler tooltip text. Default: "".
	\endlist

	\section1 Example

	\qml

	ColorPicker {
		name: "color"
		title: qsTr("Colot")
		defaultValue: "red"
	}

	\endqml
*/
ColorPickerBase {
	id: colorPicker


	property bool setButtonOnder:	false
	property alias textField:		inputField
	property alias button:			colorButton
	property alias colorDialog:		colorDialog
	property alias label:			inputField.label
	property alias buttonText:		colorButton.text
	property alias buttonInfo:		colorButton.info
	property alias value:			inputField.value

	implicitHeight: colorButton.y + colorButton.height
	implicitWidth: colorButton.x + colorButton.width

	TextField
	{
		id:			inputField
		value:		jaspTheme.jaspBlue
		fieldWidth: 80 * jaspTheme.uiScale
		isBound:	false
		onValueChanged:
		{
			colorButton.control.color = inputField.value
			colorDialog.selectedColor = inputField.value
		}
	}

	Button
	{
		id:				colorButton
		anchors.top:	colorPicker.setButtonOnder ? inputField.bottom	: inputField.top
		anchors.left:	colorPicker.setButtonOnder ? inputField.left	: inputField.right
		anchors.leftMargin: colorPicker.setButtonOnder ? inputField.control.x : 0
		text:			qsTr("Click")
		control.color:	inputField.value
		height:			inputField.height
		width:			colorPicker.setButtonOnder ? inputField.fieldWidth : Math.max(implicitWidth, height)
		onClicked:		colorDialog.open()
		info:			qsTr("Click to pick up a color")
	}

	ColorDialog
	{
		id: colorDialog
		title: qsTr("Select the color")
		onAccepted: inputField.value = colorDialog.selectedColor
	}
}
