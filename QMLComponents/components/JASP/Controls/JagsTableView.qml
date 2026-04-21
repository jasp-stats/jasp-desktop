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
    \qmltype JagsTableView
    \inqmlmodule JASP.Controls 1.0
    \brief A table view preset for entering JAGS data.

    Extends BasicThreeButtonTableView with the JAGSDataInput model type.
    Buttons are configured as Add Data, Delete Data, and Reset. Cell values
    are strings. Row count is capped by maxDataEntries.

    \section1 R Binding

    \list
    \li \b{R Type:} data.frame
    \li \b{Default:} Empty table with 2 columns
    \endlist

    \section1 Properties

    \list
    \li \b maxDataEntries (int) - Maximum number of data rows allowed. Default: 30.
    \endlist

    \section1 Inherited Properties from BasicThreeButtonTableView

    \list
    \li \b name (string) - R option name this control binds to. Default: "".
    \li \b source (var) - Source for populating the table.
    \li \b initialColumnCount (int) - Number of columns at creation. Default: 2.
    \li \b initialRowCount (int) - Number of rows at creation. Default: 0.
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
    JagsTableView {
        name: "dataInput"
        maxDataEntries: 50
    }
    \endqml
*/
BasicThreeButtonTableView
{
	id					: jagsTableView

	modelType			: JASP.JAGSDataInputModel
	itemType			: JASP.String
	initialColumnCount	: 2
	initialRowCount		: 0

	property	int		maxDataEntries  	: 30


	buttonAddText		: qsTr("Add Data")
	onAddClicked		: tableView.addRow()
	buttonAddEnabled	: tableView.columnCount > 0 && tableView.rowCount < maxDataEntries

	buttonDeleteText	: qsTr("Delete Data")
	onDeleteClicked		: tableView.removeARow()
	buttonDeleteEnabled	: tableView.rowCount > 0

	buttonResetText		: qsTr("Reset")
	onResetClicked		: tableView.reset()
	buttonResetEnabled	: tableView.rowCount > 0
}
