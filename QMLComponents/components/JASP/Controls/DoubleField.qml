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
    \qmltype DoubleField
    \inqmlmodule JASP.Controls 1.0
    \brief A text field preset for entering numeric (double) values.

    Extends TextField with the "number" input type and a built-in JASPDoubleValidator.
    Supports configurable minimum, maximum, decimal precision, and sign constraints.

    \section1 R Binding

    \list
    \li \b{R Type:} \c numeric
    \li \b{Default:} 0
    \endlist

    \section1 Properties

    \list
    \li \b negativeValues (bool) - Allow negative numbers. Default: false.
    \li \b min (double) - Minimum allowed value. Default: 0 (or -Infinity when negativeValues is true).
    \li \b max (double) - Maximum allowed value. Default: Infinity.
    \li \b decimals (int) - Number of decimal places. Default: 3.
    \li \b inclusive (enum) - Whether min/max bounds are inclusive. Default: JASP.MinMax. Can have also the values JASP.MinOnly, JASP.MaxOnly or JASP.None.
    \endlist

    \section1 Inherited Properties from TextField

    \list
    \li \b name (string) - R option name this control binds to. Default: "".
    \li \b value (string) - Current text value. Default: "".
    \li \b defaultValue (var) - Default value. Default: 0.
    \li \b label (string) - Label displayed before the field. Default: "".
    \li \b afterLabel (string) - Label displayed after the field. Default: "".
    \li \b fieldWidth (int) - Width of the input field. Default: 40.
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
    DoubleField {
        name: "alpha"
        label: qsTr("Significance level")
        defaultValue: 0.05
        min: 0
        max: 1
        decimals: 3
    }
    \endqml
*/
TextField
{
					id:					doubleField
					defaultValue:		0
	property alias	doubleValidator:	doubleValidator
	property bool	negativeValues:		false
	property double	min:				negativeValues ? -Infinity : 0
	property double	max:				Infinity
	property int	decimals:			3
	property alias	inclusive:			doubleValidator.inclusive

					inputType:			"number"
					validator:			JASPDoubleValidator { id: doubleValidator; bottom: min; top: max ; decimals: doubleField.decimals; notation: DoubleValidator.StandardNotation }
					fieldWidth:			jaspTheme.numericFieldWidth

	Accessible.description:		info === undefined || info == "" ? toolTip !== undefined && toolTip != "" ? toolTip :  qsTr("Double entry field %1").arg(title) : info
}
