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

Item
{
	id				: chi2TestTableView
	width			: parent.width
	implicitWidth	: width
	height			: 200
	implicitHeight	: height

	property	alias	name				: tableView.name
	property	alias	source				: tableView.source
	property	alias	tableView			: tableView
	property	alias	showAddButton		: addButton.visible
	property	alias	showDeleteButton	: deleteButton.visible
	property	string	tableType			: "ExpectedProportions"
	property	string	itemType			: "double"

	RowLayout
	{
		id: layout

		TableView
		{
			id				: tableView
			implicitWidth	: chi2TestTableView.width * 3 / 4 - layout.spacing
			implicitHeight	: chi2TestTableView.height
			modelType		: "MultinomialChi2Model"
			itemType		: chi2TestTableView.itemType
			tableType		: chi2TestTableView.tableType
		}

		Group
		{
			implicitWidth	: chi2TestTableView.width * 1 / 4
			implicitHeight	: chi2TestTableView.height

			Button
			{
				id				: addButton
				text			: qsTr("Add Column")
				name			: "addButton"
				control.width	: chi2TestTableView.width * 1 / 4
				onClicked		: tableView.addColumn()
				enabled			: tableView.columnCount > 0
			}

			Button
			{
				id				: deleteButton
				text			: qsTr("Delete Column")
				name			: "deleteButton"
				control.width	: chi2TestTableView.width * 1 / 4
				onClicked		: tableView.removeAColumn()
				enabled			: tableView.columnCount > 1
			}

			Button
			{
				text			: qsTr("Reset")
				name			: "resetButton"
				control.width	: chi2TestTableView.width * 1 / 4
				onClicked		: tableView.reset()
				enabled			: tableView.columnCount > 0
			}
		}
	}
}
