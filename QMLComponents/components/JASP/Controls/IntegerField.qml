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
    \qmltype IntegerField
    \inqmlmodule JASP.Controls 1.0
    \brief A text field preset for entering integer values.

    Extends TextField with the "integer" input type and a JASPDoubleValidator
    configured for zero decimal places. Supports configurable minimum, maximum,
    and sign constraints.

    \section1 R Binding

    \list
    \li \b{R Type:} \c integer
    \li \b{Default:} 0
    \endlist

    \section1 Properties

    \list
    \li \b negativeValues (bool) - Allow negative integers. Default: false.
    \li \b min (int) - Minimum allowed value. Default: 0 (or -2147483647 when negativeValues is true).
    \li \b max (int) - Maximum allowed value. Default: 2147483647.
    \li \b inclusive (enum) - Whether min/max bounds are inclusive. Default: JASP.MinMax. Can have also the values JASP.MinOnly, JASP.MaxOnly or JASP.None.
    \endlist

    \section1 Inherited Properties from TextField

    \list
    \li \b name (string) - R option name this control binds to. Default: "".
    \li \b value (string) - Current text value. Default: "".
    \li \b defaultValue (var) - Default value. Default: 0.
    \li \b label (string) - Label displayed before the field. Default: "".
    \li \b afterLabel (string) - Label displayed after the field. Default: "".
    \li \b fieldWidth (int) - Width of the input field. Default: jaspTheme.numericFieldWidth.
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
    IntegerField {
        name: "sampleSize"
        label: qsTr("Sample size")
        defaultValue: 100
        min: 1
    }
    \endqml
*/
TextField
{
					id:					textField
					defaultValue:		0
	property bool	negativeValues:		false
	property int	min:				negativeValues ? -2147483647 : 0 // 2^32 - 1
	property int	max:				2147483647
	property alias	inclusive:			intValidator.inclusive
	property alias	intValidator:		intValidator
    
					inputType:			"integer"
					validator:			JASPDoubleValidator { id: intValidator; bottom: min; top: max; decimals: 0 }
					cursorShape:		Qt.IBeamCursor
					fieldWidth:			jaspTheme.numericFieldWidth
					
	Accessible.description:		info === undefined || info == "" ? toolTip !== undefined && toolTip != "" ? toolTip :  qsTr("Integer entry field %1").arg(title) : info
}
