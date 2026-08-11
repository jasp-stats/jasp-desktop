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
    \qmltype ComputedColumnField
    \inqmlmodule JASP.Controls 1.0
    \brief A text field for entering a computed column name.

    Extends TextField with the "computedColumn" input type. The entered name is used
    to create or reference a computed column in the dataset. Validation ensures the
    column name is valid and available.

    \section1 R Binding

    \list
    \li \b{R Type:} \c character
    \li \b{Default:} ""
    \endlist

    \section1 Inherited Properties from TextField

    \list
    \li \b name (string) - R option name this control binds to. Default: "".
    \li \b value (string) - Current text value. Default: "".
    \li \b label (string) - Label displayed before the field. Default: "".
    \li \b afterLabel (string) - Label displayed after the field. Default: "".
    \li \b fieldWidth (int) - Width of the input field. Default: 200.
    \li \b placeholderText (string) - Greyed text shown when field is empty. Default: "".
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
    ComputedColumnField {
        name: "generatedColumn"
        label: qsTr("Column name")
    }
    \endqml
*/
TextField
{
	inputType:			"computedColumn"
	
	Accessible.description:		info === undefined || info == "" ? toolTip !== undefined && toolTip != "" ? toolTip :  qsTr("Add a computed column textfield %1").arg(title) : info
}
