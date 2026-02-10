//
// Copyright (C) 2013-2020 University of Amsterdam
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
import QtQuick.Layouts
import JASP.Controls
import JASP

/*!
    \qmltype CustomContrastsTableView
    \inqmlmodule JASP.Controls 1.0
    \brief A table view preset for entering custom contrast weight matrices.

    Extends BasicThreeButtonTableView with the CustomContrasts model type.
    Buttons are configured as Add Contrast, Delete Contrast, and Reset, placed in a row
    above the table. Rows are automatically populated from the factor levels.

    \section1 R Binding

    \list
    \li \b{R Type:} matrix
    \li \b{Default:} Identity contrast matrix matching factor levels
    \endlist

    \section1 Inherited Properties from BasicThreeButtonTableView

    \list
    \li \b name (string) - R option name this control binds to. Default: "".
    \li \b source (var) - Source for populating the table.
    \li \b factorsSource (var) - Source providing factor level information for row headers.
    \li \b itemType (var) - Data type for cell values. Default: JASP.Double.
    \li \b minimum (var) - Minimum allowed value for cells. Default: -Infinity.
    \li \b decimals (int) - Number of decimal places. Default: 3.
    \li \b columnName (string) - Column name alias for the contrast variable.
    \li \b buttonsInRow (bool) - Place buttons in a row above the table. Default: true.
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
    CustomContrastsTableView {
        name: "values"
        columnName: "myFactor"
        factorsSource: "repeatedMeasuresFactors"
    }
    \endqml
*/
BasicThreeButtonTableView
{
	id				: customContrastsTV

	preferredHeight	: Math.max(150 * preferencesModel.uiScale,10 * preferencesModel.uiScale + tableView.y + tableView.tableHeight)

	modelType			: JASP.CustomContrasts
	itemType			: JASP.Double
	minimum				: -Infinity
	initialColumnCount	: 1
	initialRowCount		: 0
	buttonsInRow		: true

	cornerText			: ""

	buttonAddText		: qsTr("Add Contrast")
    onAddClicked		: tableView.addColumn()

	buttonDeleteText	: qsTr("Delete Contrast")
    onDeleteClicked		: tableView.removeAColumn()
    buttonDeleteEnabled	: tableView.columnCount > (tableView.variableCount + 1)

	buttonResetText		: qsTr("Reset")
	onResetClicked		: tableView.reset()
    buttonResetEnabled	: tableView.columnCount > (tableView.variableCount + 1)
}
