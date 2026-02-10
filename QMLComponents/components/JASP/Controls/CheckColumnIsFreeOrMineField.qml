//
// Copyright (C) 2013-2024 University of Amsterdam
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
    \qmltype CheckColumnIsFreeOrMineField
    \inqmlmodule JASP.Controls 1.0
    \brief A text field that validates whether a column name is free or owned by the current analysis.

    Extends TextField with the "checkColumn" input type, which ensures the entered column name
    is either not yet used in the dataset or already belongs to the current analysis.
    Typically used when an analysis creates or claims a computed column.

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
    CheckColumnIsFreeOrMineField {
        name: "computedColumn"
        label: qsTr("Column name")
    }
    \endqml
*/
TextField
{
	inputType:			"checkColumn"
}
