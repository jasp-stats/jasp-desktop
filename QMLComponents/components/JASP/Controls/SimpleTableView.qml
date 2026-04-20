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
    \qmltype SimpleTableView
    \inqmlmodule JASP.Controls 1.0
    \brief A table view preset with Add Column, Delete Column, and Reset buttons.

    Extends BasicThreeButtonTableView with the Simple model type.
    Columns can be added and removed dynamically. Reset restores the
    initial column layout.

    \section1 R Binding

    \list
    \li \b{R Type:} data.frame
    \li \b{Default:} Table with initialColumnCount columns and initialRowCount rows
    \endlist

    \section1 Inherited Properties from BasicThreeButtonTableView

    \list
    \li \b name (string) - R option name. Default: "".
    \li \b source (var) - Source for populating the table.
    \li \b initialColumnCount (int) - Starting column count. Default: 1.
    \li \b initialRowCount (int) - Starting row count. Default: 0.
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
    SimpleTableView {
        name: "priorMatrix"
        initialColumnCount: 3
        initialRowCount: 3
    }
    \endqml
*/
BasicThreeButtonTableView
{
	modelType		: JASP.Simple

    buttonAddText		: qsTr("Add Column")
    onAddClicked		: tableView.addColumn()
    buttonAddEnabled	: true

    buttonDeleteText	: qsTr("Delete Column")
    onDeleteClicked		: tableView.removeAColumn()
	buttonDeleteEnabled	: tableView.columnCount > initialColumnCount

    buttonResetText		: qsTr("Reset")
    onResetClicked		: tableView.reset()
	buttonResetEnabled	: tableView.columnCount > initialColumnCount
}
