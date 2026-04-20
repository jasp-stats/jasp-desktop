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

import JASP

/*!
    \qmltype AssignedRepeatedMeasuresCells
    \inqmlmodule JASP.Controls 1.0
    \brief A variable list preset for assigning variables to repeated measures cells.

    Extends VariablesList with a two-column layout designed for repeated measures designs.
    Uses drop-replace mode and restricts to scale variables only.
    Typically used within a VariablesForm alongside a FactorsForm that defines the repeated measures factors.

    \section1 R Binding

    \list
    \li \b{R Type:} list
    \li \b{Default:} [] (empty array)
    \endlist

    \section1 Inherited Properties from VariablesList

    \list
    \li \b name (string) - R option name this control binds to. Default: "".
    \li \b title (string) - Title displayed above the list. Alias: label. Default: "".
    \li \b allowedColumns (array) - Restrict to column types: "scale", "ordinal", "nominal". Default: [].
    \li \b showVariableTypeIcon (bool) - Display variable type icons. Default: false.
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
    AssignedRepeatedMeasuresCells {
        name: "repeatedMeasuresCells"
        title: qsTr("Repeated Measures Cells")
    }
    \endqml
*/
VariablesList
{
	listViewType			: JASP.RepeatedMeasures
	dropMode				: JASP.DropReplace
	showElementBorder		: true
	columns					: 2
	showVariableTypeIcon	: false
	allowedColumns			: ["scale"]
	height					: 140
}
