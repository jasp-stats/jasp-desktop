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
    \qmltype AvailableVariablesList
    \inqmlmodule JASP.Controls 1.0
    \brief The source list showing all available dataset variables.

    Extends VariablesList configured as the source (available) list in a VariablesForm.
    This control is not bound to R options. It displays all dataset variables
    from which users can drag variables into assigned lists.

    \note AvailableVariablesList does not bind to R options.
    It is automatically managed by VariablesForm.

    \section1 Inherited Properties from VariablesList

    \list
    \li \b title (string) - Title displayed above the list. Alias: label. Default: "".
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
    VariablesForm {
        AvailableVariablesList {
            name: "allVariables"
        }
        AssignedVariablesList {
            name: "dependent"
            singleVariable: true
        }
    }
    \endqml
*/
VariablesList
{
	listViewType:		JASP.AvailableVariables
	showSortMenu:		true
	isBound:			false
	allowTypeChange:	false
}
