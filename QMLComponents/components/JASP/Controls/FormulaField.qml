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
import JASP

/*!
    \qmltype FormulaField
    \inqmlmodule JASP.Controls 1.0
    \brief A text field preset for entering R-style formulas.

    Extends TextField with the "formula" input type. Formulas are evaluated by the
    R engine and validated against configurable min/max bounds. The computed numeric
    result is accessible via realValue.

    \section1 R Binding

    \list
    \li \b{R Type:} \c character (formula string, evaluated to numeric)
    \li \b{Default:} "0"
    \endlist

    \section1 Properties

    \list
    \li \b realValue (double) - The evaluated numeric result of the formula. Read-only.
    \li \b realValues (array) - Array of evaluated results when formula yields multiple values. Read-only.
    \li \b min (double) - Minimum allowed evaluated value. Default: -Infinity.
    \li \b max (double) - Maximum allowed evaluated value. Default: Infinity.
    \li \b inclusive (enum) - Whether min/max bounds are inclusive. Default: JASP.MinMax. Can have also the values JASP.MinOnly, JASP.MaxOnly or JASP.None.
    \li \b parseDefaultValue (bool) - Whether the default value should be parsed as a formula. Default: true.
    \endlist

    \section1 Inherited Properties from TextField

    \list
    \li \b name (string) - R option name this control binds to. Default: "".
    \li \b value (string) - Current formula text. Default: "".
    \li \b defaultValue (var) - Default formula text. Default: "0".
    \li \b label (string) - Label displayed before the field. Default: "".
    \li \b afterLabel (string) - Label displayed after the field. Default: "".
    \li \b fieldWidth (int) - Width of the input field. Default: jaspTheme.textFieldWidth / 2.
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
    FormulaField { // The user can here type '1/3' or 'sin(10)'
        name: "priorMean"
        label: qsTr("Prior mean")
        defaultValue: "0"
    }
    \endqml
*/
TextField
{
					id:					formulaField
					defaultValue:		"0"
	property double realValue:			0
	property var	realValues:			[]
	property double	min:				-Infinity
	property double	max:				Infinity
	property int	inclusive:			JASP.MinMax
	property bool	parseDefaultValue:	true
					inputType:			"formula"
					fieldWidth:			jaspTheme.textFieldWidth / 2
}
