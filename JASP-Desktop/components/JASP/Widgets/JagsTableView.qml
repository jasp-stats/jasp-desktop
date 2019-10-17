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

Item
{
	id				: jagsTableView
	width			: implicitWidth
	implicitWidth	: parent.width
	height			: implicitHeight
	implicitHeight	: 200 * preferencesModel.uiScale

	property	alias	name				: tableView.name
	property	alias	source				: tableView.source
	property	alias	tableView			: tableView
	property	alias	showAddButton		: addButton.visible
	property	alias	showDeleteButton	: deleteButton.visible
	property	string	tableType			: "userDataInput"
	property	string	itemType			: "string"
	property	int		maxDataEntries  	: 30

	RowLayout
	{
		id: layout

		TableView
		{
			id					: tableView
			implicitWidth		: jagsTableView.tableType == "userDataInput" ? jagsTableView.width * 3 / 4 - layout.spacing : jagsTableView.width
			implicitHeight		: jagsTableView.height
			modelType			: "JAGSDataInputModel"
			itemType			: jagsTableView.itemType
			tableType			: jagsTableView.tableType
			initialColumnCount	: 2
			initialRowCount		: 0
		}

		Group
		{
            visible         : jagsTableView.tableType == "userDataInput"
			implicitWidth	: jagsTableView.width * 1 / 4
			implicitHeight	: jagsTableView.height

			Button
			{
				id				: addButton
				text			: qsTr("Add Data")
				name			: "addButton"
				control.width	: jagsTableView.width * 1 / 4
				onClicked		: tableView.addRow()
				enabled			: (tableView.columnCount > 0 && tableView.rowCount < maxDataEntries)
			}

			Button
			{
				id				: deleteButton
				text			: qsTr("Delete Data")
				name			: "deleteButton"
				control.width	: jagsTableView.width * 1 / 4
				onClicked		: tableView.removeARow()
				enabled			: tableView.rowCount > 1
			}

			Button
			{
				text			: qsTr("Reset")
				name			: "resetButton"
				control.width	: jagsTableView.width * 1 / 4
				onClicked		: tableView.reset()
				enabled			: tableView.rowCount > 0
			}
		}
	}
}
