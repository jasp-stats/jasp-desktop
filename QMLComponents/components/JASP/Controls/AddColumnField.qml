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
    \qmltype AddColumnField
    \inqmlmodule JASP.Controls 1.0
    \brief A text input field that creates a new computed column in the dataset.

    Extends TextField with inputType set to "addColumn". The entered name becomes
    the name of a new column added to the dataset by the analysis.

    \list
    \li \b{R Type:} \c character
    \li \b{Default:} ""
    \endlist

    \section1 Properties

    \list
    \li \b name (string) - R option name this control binds to. Default: "".
    \li \b columnType (int) - The type of the new column: columnTypeScale, columnTypeNominal, or columnTypeOrdinal. Default: columnTypeScale.
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
    AddColumnField {
        name: "residuals"
        columnType: columnTypeScale
    }
    \endqml
*/
TextField
{
	inputType:			"addColumn"

	property int columnType: columnTypeScale //Or columnTypeNominal, or columnTypeOrdinal
}
