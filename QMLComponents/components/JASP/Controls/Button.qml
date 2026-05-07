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

/*!
    \qmltype Button
    \inqmlmodule JASP.Controls 1.0
    \brief A generic clickable button.

    \note Button does not bind to R options. It is used for triggering actions in the UI.

    \section1 Properties

    \list
    \li \b text (string) - Button label text. Alias: label. Default: "".
    \li \b iconSource (string) - Path to an icon displayed on the button.
    \endlist

    \section1 Inherited Properties

    \list
    \li \b enabled (bool) - Whether the control is interactive. Default: true.
    \li \b visible (bool) - Whether the control is visible. Default: true.
    \li \b info (string) - Info that will be used by tooltip and to generate the help. Default: "".
    \li \b toolTip (string) - This property overwrite info property, in order to display a simpler tooltip text. Default: "".
    \endlist

    \section1 Signals

    \list
    \li \b clicked() - Emitted when the button is clicked.
    \endlist

    \section1 Example

    \qml
    Button {
        text: qsTr("Run Analysis")
    }
    \endqml
*/
JASPControl
{
	id:					button
	controlType:		JASPControl.Button
	implicitHeight:		control.implicitHeight
	implicitWidth:		control.implicitWidth
	isBound:			false
	shouldStealHover:	false
	innerControl:		control
	focusIndicator:		focusIndicator
	title:				text

	
	readonly	property alias control:		control
				property alias text:		control.text
				property alias label:		control.text
				property alias textFormat:	control.textFormat
				property alias iconSource:	control.iconSource
	readonly	property alias pressed:		control._pressed

    signal clicked()
    
	Component.onCompleted: control.clicked.connect(clicked);

	RoundedButton
	{
		id:				control
		anchors.fill:	parent
		focus:			true
    }

	Rectangle
	{
		id:					focusIndicator
		anchors.fill:		parent
		color:				"transparent"
		border.width:		0
		border.color:		"transparent"
		radius:				jaspTheme.jaspControlHighlightWidth
	}
}
