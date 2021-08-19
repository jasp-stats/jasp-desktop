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
	implicitHeight	: Math.max(150 * preferencesModel.uiScale, (basicButtonTableView.showButtons ? buttonColumn.height : 0))

	property int preferredHeight:	implicitHeight
	property int preferredWidth:	implicitWidth

	Layout.preferredWidth:	preferredWidth
	Layout.preferredHeight:	preferredHeight

	property	bool	buttonsInRow		: false
	property	alias	name				: tableView.name
    property	alias	source				: tableView.source
    property	alias	values				: tableView.values
    property	alias	tableView			: tableView
	property	alias	factorsSource		: tableView.factorsSource
	property	alias	control				: tableView //Needed for RowComponent

	property	alias	itemType			: tableView.itemType
	property	alias	itemTypePerColumn	: tableView.itemTypePerColumn
	property	alias	itemTypePerRow		: tableView.itemTypePerRow
	property	alias	modelType			: tableView.modelType

	property	alias	cornerText			: tableView.cornerText
	property	alias	minimum				: tableView.minimum
	property	alias	initialColumnCount	: tableView.initialColumnCount
	property	alias	initialRowCount		: tableView.initialRowCount
	property	alias	columnName			: tableView.colName
	property	alias	decimals			: tableView.decimals
	property	alias	scaleFactor			: tableView.scaleFactor
	property	alias	isFirstColEditable	: tableView.isFirstColEditable
	property	alias	columnNames			: tableView.columnNames
	property	alias	rowNames			: tableView.rowNames

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

	function getColHeaderText(headerText, columnIndex)			{ return (columnNames.length > columnIndex)	? columnNames[columnIndex]	: headerText; }
	function getRowHeaderText(headerText, rowIndex)				{ return (rowNames.length > rowIndex)		? rowNames[rowIndex]		: headerText; }

	Grid
	{
		id					: buttonColumn
		columns				: buttonsInRow ? 3 : 1
		rows				: buttonsInRow ? 1 : 3
		anchors.top			: parent.top
		anchors.left		: parent.left
		anchors.leftMargin	: jaspTheme.generalAnchorMargin
		width				: basicButtonTableView.showButtons ? (basicButtonTableView.width * (buttonsInRow ? 1 : 1 / 4) - jaspTheme.generalAnchorMargin * 2) : 0
		//height				: showButtons ? (buttonsInRow ? jaspTheme.defaultRectangularButtonHeight : jaspTheme.defaultRectangularButtonHeight * 3 + spacing * 2) : 0
		spacing				: jaspTheme.columnGroupSpacing
		visible				: basicButtonTableView.showButtons

		RoundedButton
		{
			id				: addButton
			text			: qsTr("Add")
			width			: buttonsInRow ? basicButtonTableView.width * 1/4 - jaspTheme.generalAnchorMargin * 2 : buttonColumn.width
			onClicked		: { forceActiveFocus(); basicButtonTableView.addClicked() }
		}

		RoundedButton
		{
			id				: deleteButton
			text			: qsTr("Delete")
			width			: buttonsInRow ? basicButtonTableView.width * 1/4 - jaspTheme.generalAnchorMargin * 2 : buttonColumn.width
			onClicked		: { forceActiveFocus(); basicButtonTableView.deleteClicked() }
		}

		RoundedButton
		{
			id				: resetButton
			text			: qsTr("Reset")
			width			: buttonsInRow ? basicButtonTableView.width * 1/4 - jaspTheme.generalAnchorMargin * 2 : buttonColumn.width
			onClicked		: { forceActiveFocus(); basicButtonTableView.resetClicked() }
		}
	}

	TableView
	{
		id				: tableView

		anchors.top		: buttonColumn.visible && buttonsInRow ? buttonColumn.bottom : parent.top
		anchors.left	: buttonColumn.visible && !buttonsInRow ? buttonColumn.right : parent.left
		anchors.leftMargin: jaspTheme.generalAnchorMargin
		anchors.topMargin: buttonColumn.visible && buttonsInRow ? jaspTheme.generalAnchorMargin : 0
		width			: tableView.tableWidth  < maxWidth  ? tableView.tableWidth : maxWidth
		height			: tableView.tableHeight < maxHeight ? tableView.tableHeight : maxHeight

		property int maxWidth	: basicButtonTableView.width * ((basicButtonTableView.showButtons && !buttonsInRow) ? (3 / 4) : 1)
		property int maxHeight	: basicButtonTableView.height

		function getColHeaderText(defaultName, colIndex) { return basicButtonTableView.getColHeaderText(defaultName, colIndex); }
		function getRowHeaderText(defaultName, rowIndex) { return basicButtonTableView.getRowHeaderText(defaultName, rowIndex); }

		Component.onCompleted	: basicButtonTableView.tableViewCompleted()
	}

}
