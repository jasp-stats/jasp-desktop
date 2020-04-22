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


import QtQuick 2.0
import QtQuick.Layouts 1.11
import JASP.Controls 1.0

BasicThreeButtonTableView
{
	id				: customContrastsTV

	preferredHeight	: Math.max(150 * preferencesModel.uiScale,10 * preferencesModel.uiScale + tableView.y + tableView.tableHeight)

	tableType			: "userDataInput"
	itemType			: "string"
	modelType			: "MarginalMeansContrasts"
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
