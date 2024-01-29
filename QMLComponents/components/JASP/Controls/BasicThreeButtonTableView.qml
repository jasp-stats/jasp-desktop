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

Item
{
	id				: basicButtonTableView
	implicitWidth	: buttonsInRow ? Math.max(tableView.implicitWidth, buttonColumn.width) : (tableView.x + tableView.implicitWidth)
	implicitHeight	: buttonsInRow ? (tableView.y + tableView.implicitHeight) : Math.max(tableView.implicitHeight, buttonColumn.height)

	property int preferredHeight:	implicitHeight
	property int preferredWidth:	parent.width

	Layout.preferredWidth:	preferredWidth
	Layout.preferredHeight:	preferredHeight
	Layout.columnSpan: (parent && parent.hasOwnProperty('columns')) ? parent.columns : 1


	property	bool	buttonsInRow		: false
	property	alias	name				: tableView.name
    property	alias	source				: tableView.source
    property	alias	values				: tableView.values
    property	alias	tableView			: tableView
	property	alias	factorsSource		: tableView.factorsSource
	property	alias	control				: tableView //Needed for RowComponent
	property	alias	info				: tableView.info

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
	property	alias	defaultValue		: tableView.defaultValue

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
	function getDefaultValue(columnIndex, rowIndex)				{ return defaultValue						}
	function getValidator(columnIndex, rowIndex)				{ return tableView.validator				}
	function getEditable(columnIndex, rowIndex)					{ return true								}

	Grid
	{
		id					: buttonColumn
		columns				: buttonsInRow ? 3 : 1
		anchors.top			: parent.top
		anchors.left		: parent.left
		width				: basicButtonTableView.showButtons ? (basicButtonTableView.preferredWidth * (buttonsInRow ? 1 : 1 / 4) - jaspTheme.generalAnchorMargin) : 0
		//height				: showButtons ? (buttonsInRow ? jaspTheme.defaultRectangularButtonHeight : jaspTheme.defaultRectangularButtonHeight * 3 + spacing * 2) : 0
		spacing				: jaspTheme.columnGroupSpacing
		visible				: basicButtonTableView.showButtons

		property int buttonWidth: buttonsInRow ? basicButtonTableView.preferredWidth * 1/4 - jaspTheme.generalAnchorMargin : buttonColumn.width

		RoundedButton
		{
			id				: addButton
			text			: qsTr("Add")
			width			: buttonColumn.buttonWidth
			onClicked		: { forceActiveFocus(); basicButtonTableView.addClicked() }
		}

		RoundedButton
		{
			id				: deleteButton
			text			: qsTr("Delete")
			width			: buttonColumn.buttonWidth
			onClicked		: { forceActiveFocus(); basicButtonTableView.deleteClicked() }
		}

		RoundedButton
		{
			id				: resetButton
			text			: qsTr("Reset")
			width			: buttonColumn.buttonWidth
			onClicked		: { forceActiveFocus(); basicButtonTableView.resetClicked() }
		}
	}

	TableView
	{
		id					: tableView
		anchors.top			: buttonsAbove	? buttonColumn.bottom	: parent.top
		anchors.left		: bottonsLeft	? buttonColumn.right	: parent.left
		anchors.topMargin	: buttonsAbove	? jaspTheme.generalAnchorMargin : 0
		anchors.leftMargin	: bottonsLeft	? jaspTheme.generalAnchorMargin : 0
		width				: Math.min(tableView.tableWidth, maxWidth)
		height				: Math.min(tableView.tableHeight, maxHeight)

		property bool buttonsAbove	: buttonColumn.visible && buttonsInRow
		property bool bottonsLeft	: buttonColumn.visible && !buttonsInRow
		property int maxWidth		: basicButtonTableView.preferredWidth * (bottonsLeft ? (3 / 4) : 1)
		property int maxHeight		: basicButtonTableView.preferredHeight - tableView.y

		function getColHeaderText(defaultName, colIndex)	{ return basicButtonTableView.getColHeaderText(defaultName, colIndex)	}
		function getRowHeaderText(defaultName, rowIndex)	{ return basicButtonTableView.getRowHeaderText(defaultName, rowIndex)	}
		function getDefaultValue(columnIndex, rowIndex)		{ return basicButtonTableView.getDefaultValue(columnIndex, rowIndex)	}
		function getValidator(columnIndex, rowIndex)		{ return basicButtonTableView.getValidator(columnIndex, rowIndex)		}
		function getEditable(columnIndex, rowIndex)			{ return basicButtonTableView.getEditable(columnIndex, rowIndex)		}

		Component.onCompleted	: basicButtonTableView.tableViewCompleted()
	}

}
