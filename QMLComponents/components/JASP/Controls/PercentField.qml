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
    \qmltype PercentField
    \inqmlmodule JASP.Controls 1.0
    \brief A numeric field preset for entering percentage values (0–100).

    Extends DoubleField with the "percent" input type, 0 decimal places,
    and a "%" suffix label. Min is 0, max is 100.

    \section1 R Binding

    \list
    \li \b{R Type:} \c numeric (percentage)
    \li \b{Default:} 50
    \endlist

    \section1 Properties

    \list
    \li \b showPercent (bool) - Show the "%" suffix after the field. Default: true.
    \endlist

    \section1 Inherited Properties from DoubleField

    \list
    \li \b name (string) - R option name this control binds to. Default: "".
    \li \b defaultValue (var) - Default value. Default: 50.
    \li \b min (double) - Minimum value. Default: 0.
    \li \b max (double) - Maximum value. Default: 100.
    \li \b decimals (int) - Decimal places. Default: 0.
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
    PercentField {
        name: "confidenceLevel"
        label: qsTr("Confidence interval")
        defaultValue: 95
    }
    \endqml
*/
DoubleField
{
	id:					percentField
	property bool		showPercent:	true

	defaultValue:		50
	decimals:			0
	min:				0
	max:				100
	inputType:			"percent"
	fieldWidth:			jaspTheme.font.pixelSize * (percentField.decimals + 3)

	afterLabel:			showPercent ? "%" : ""
	cursorShape:		Qt.IBeamCursor	
	
	Accessible.description:		info === undefined || info == "" ? toolTip !== undefined && toolTip != "" ? toolTip :  qsTr("Percentage field %1").arg(title) : info
}
