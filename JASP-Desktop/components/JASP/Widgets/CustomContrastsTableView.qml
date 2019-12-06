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
	id				: customContrastsTV
	width			: parent.width
	implicitWidth	: width
	height			: 200 * preferencesModel.uiScale
	implicitHeight	: height

	property	alias	control				: tableView // needed for RowComponents
	property	alias	name				: tableView.name
	property	alias	columnName			: tableView.colName
	property	alias	source				: tableView.source
	property	alias	factorsSource		: tableView.factorsSource
	property	alias	tableView			: tableView
	property	alias	showAddButton		: addButton.visible
	property	alias	showDeleteButton	: deleteButton.visible
	property	int		maxNumHypotheses	: 10

	RowLayout
	{
		id: layout

		TableView
		{
			id					: tableView
			implicitWidth		: customContrastsTV.width * 3 / 4 - layout.spacing
			implicitHeight		: customContrastsTV.height
			modelType			: "CustomContrasts"
			itemType			: "double"
			minimum				: -Infinity
			cornerText			: qsTr("Contrast #")

			Component.onCompleted: tableView.rowNumberWidth = 80 * preferencesModel.uiScale
		}

		Group
		{
			implicitWidth	: customContrastsTV.width * 1 / 4
			implicitHeight	: customContrastsTV.height

			Button
			{
				id				: addButton
				text			: qsTr("Add Contrast")
				name			: "addButton"
				control.width	: customContrastsTV.width * 1 / 4
				onClicked		: tableView.addRow()
			}

			Button
			{
				id				: deleteButton
				text			: qsTr("Delete Contrast")
				name			: "deleteButton"
				control.width	: customContrastsTV.width * 1 / 4
				onClicked		: tableView.removeARow()
				enabled			: tableView.rowCount > 1
			}

			Button
			{
				text			: qsTr("Reset")
				name			: "resetButton"
				control.width	: customContrastsTV.width * 1 / 4
				onClicked		: tableView.reset()
				enabled			: tableView.rowCount > 0
			}
		}
	}
}
