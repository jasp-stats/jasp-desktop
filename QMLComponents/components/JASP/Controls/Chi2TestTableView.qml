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
import QtQuick.Layouts
import JASP.Controls
import JASP

/*!
    \qmltype Chi2TestTableView
    \inqmlmodule JASP.Controls 1.0
    \brief A table view preset for entering multinomial chi-squared hypotheses.

    Extends BasicThreeButtonTableView with the MultinomialChi2 model type.
    The three buttons are configured as Add Column, Delete Column, and Reset.
    Column headers default to "H₀ (a)", "H₀ (b)", etc.

    \section1 R Binding

    \list
    \li \b{R Type:} matrix
    \li \b{Default:} Empty table
    \endlist

    \section1 Properties

    \list
    \li \b maxNumHypotheses (int) - Maximum number of hypothesis columns allowed. Default: 10.
    \li \b colHeader (string) - Custom column header text. When empty, uses "H₀ (a)", "H₀ (b)", etc. Default: "".
    \endlist

    \section1 Inherited Properties from BasicThreeButtonTableView

    \list
    \li \b name (string) - R option name this control binds to. Default: "".
    \li \b source (var) - Source for populating the table.
    \li \b initialRowCount (int) - Number of rows at creation.
    \li \b defaultValue (var) - Default value for new cells.
    \li \b buttonsInRow (bool) - Place buttons in a row above the table. Default: false.
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
    Chi2TestTableView {
        name: "tableWidget"
        maxNumHypotheses: 5
    }
    \endqml
*/
BasicThreeButtonTableView
{
	id				: chi2TestTableView

	modelType		: JASP.MultinomialChi2Model

	property	int		maxNumHypotheses	: 10
	property	string	colHeader			: ""

	buttonAddText		: qsTr("Add Column")
	onAddClicked		: tableView.addColumn()
	buttonAddEnabled	: (tableView.columnCount > 0 && tableView.columnCount < maxNumHypotheses)

	buttonDeleteText	: qsTr("Delete Column")
	onDeleteClicked		: tableView.removeAColumn()
	buttonDeleteEnabled	: tableView.columnCount > 1

	buttonResetText		: qsTr("Reset")
	onResetClicked		: tableView.reset()
	buttonResetEnabled	: tableView.columnCount > 0

	function getColHeaderText(headerText, columnIndex)	{ return colHeader ? colHeader : "H₀ (" + String.fromCharCode(97 + columnIndex) + ")" ; }
}
