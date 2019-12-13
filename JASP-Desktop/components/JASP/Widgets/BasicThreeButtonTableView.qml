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
	id				: basicButtonTableView
	width			: implicitWidth
	height			: implicitHeight
	implicitWidth	: parent.width
	implicitHeight	: 200 * preferencesModel.uiScale

	property	alias	name				: tableView.name
	property	alias	source				: tableView.source
	property	alias	tableView			: tableView
	property	alias	factorsSource		: tableView.factorsSource
	property	alias	control				: tableView //Needed for RowComponents

	property	alias	tableType			: tableView.tableType
	property	alias	itemType			: tableView.itemType
	property	alias	modelType			: tableView.modelType

	property	alias	cornerText			: tableView.cornerText
	property	alias	minimum				: tableView.minimum
	property	alias	initialColumnCount	: tableView.initialColumnCount
	property	alias	initialRowCount		: tableView.initialRowCount
	property	alias	columnName			: tableView.colName

	property	bool	showButtons			: true

	property	alias	buttonAddText		: addButton.text
	property	alias	buttonDeleteText	: deleteButton.text
	property	alias	buttonResetText		: resetButton.text

	property	alias	buttonAddEnabled	: addButton.enabled
	property	alias	buttonDeleteEnabled	: deleteButton.enabled
	property	alias	buttonResetEnabled	: resetButton.enabled

	property	alias	showAddButton		: addButton.visible
	property	alias	showDeleteButton	: deleteButton.visible
	property	alias	showResetButton		: resetButton.visible

	signal	addClicked();
	signal	deleteClicked();
	signal	resetClicked();

	signal tableViewCompleted();

	TableView
	{
		id				: tableView
		width			: !basicButtonTableView.showButtons ? basicButtonTableView.width :  basicButtonTableView.width * 3 / 4 - buttonColumn.anchors.leftMargin
		height			: basicButtonTableView.height

		Component.onCompleted: basicButtonTableView.tableViewCompleted();
	}

	Column
	{
		id					: buttonColumn
		anchors.left		: tableView.right
		anchors.leftMargin	: jaspTheme.generalAnchorMargin
		width				: basicButtonTableView.showButtons ? basicButtonTableView.width * 1 / 4 : 0
		height				: basicButtonTableView.height
		spacing				: jaspTheme.columnGroupSpacing
		visible				: basicButtonTableView.showButtons

		RectangularButton
		{
			id				: addButton
			text			: qsTr("Add")
			width			: basicButtonTableView.width * 1 / 4
			onClicked		: basicButtonTableView.addClicked()
		}

		RectangularButton
		{
			id				: deleteButton
			text			: qsTr("Delete")
			width			: basicButtonTableView.width * 1 / 4
			onClicked		: basicButtonTableView.deleteClicked()
		}

		RectangularButton
		{
			id				: resetButton
			text			: qsTr("Reset")
			width			: basicButtonTableView.width * 1 / 4
			onClicked		: basicButtonTableView.resetClicked()
		}
	}

}
