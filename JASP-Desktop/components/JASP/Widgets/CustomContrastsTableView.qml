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


import QtQuick 2.0
import QtQuick.Layouts 1.11
import JASP.Controls 1.0

BasicThreeButtonTableView
{
	id				: customContrastsTV

	itemType		: "double"
	modelType		: "CustomContrasts"
	cornerText		: qsTr("Level #")
	minimum			: -Infinity
	decimals		: 3


	buttonAddText		: qsTr("Add Contrast")
	onAddClicked		: tableView.addRow()

	buttonDeleteText	: qsTr("Delete Contrast")
	onDeleteClicked		: tableView.removeARow()
	buttonDeleteEnabled	: tableView.rowCount > 1

	buttonResetText		: qsTr("Reset")
	onResetClicked		: tableView.reset()
	buttonResetEnabled	: tableView.rowCount > 0
}
