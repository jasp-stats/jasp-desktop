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

/*!
    \qmltype BasicThreeButtonTableView
    \inqmlmodule JASP.Controls 1.0
    \brief A table input control with Add, Delete, and Reset buttons.

    Wraps a TableView with three action buttons for managing rows.
    Used for entering structured data such as matrices or custom data tables.

    \section1 R Binding

    \list
    \li \b{R Type:} data.frame or matrix
    \li \b{Default:} Empty table with initialRowCount rows and initialColumnCount columns
    \endlist

    \section1 Properties

    \list
    \li \b name (string) - R option name this control binds to. Default: "".
    \li \b source (var) - Source for populating the table.
    \li \b values (var) - Values to populate the table with.
    \li \b initialColumnCount (int) - Number of columns at creation.
    \li \b initialRowCount (int) - Number of rows at creation.
    \li \b cornerText (string) - Text displayed in the top-left corner cell.
    \li \b columnNames (list) - Custom column header names.
    \li \b rowNames (list) - Custom row header names.
    \li \b defaultValue (var) - Default value for new cells.
    \li \b minimum (var) - Minimum allowed value for cells.
    \li \b decimals (int) - Number of decimal places for numeric cells.
    \li \b isFirstColEditable (bool) - Whether the first column is editable.
    \li \b buttonsInRow (bool) - Place buttons in a row above the table instead of a column on the left. Default: false.
    \li \b showButtons (bool) - Show the Add/Delete/Reset buttons. Default: true.
    \li \b showAddButton (bool) - Show the Add button. Default: true.
    \li \b showDeleteButton (bool) - Show the Delete button. Default: true.
    \li \b showResetButton (bool) - Show the Reset button. Default: true.
    \endlist

    \section1 Inherited Properties

    \list
    \li \b enabled (bool) - Whether the control is interactive. Default: true.
    \li \b visible (bool) - Whether the control is visible. Default: true.
    \li \b info (string) - Info that will be used by tooltip and to generate the help. Default: "".
    \li \b toolTip (string) - This property overwrite info property, in order to display a simpler tooltip text. Default: "".
    \endlist

    \section1 Signals

    \list
    \li \b addClicked() - Emitted when the Add button is clicked.
    \li \b deleteClicked() - Emitted when the Delete button is clicked.
    \li \b resetClicked() - Emitted when the Reset button is clicked.
    \li \b tableViewCompleted() - Emitted when the internal TableView has completed initialization.
    \endlist

    \section1 Example

    \qml
    BasicThreeButtonTableView {
        name: "priorCounts"
        initialColumnCount: 3
        initialRowCount: 2
        columnNames: [qsTr("Group 1"), qsTr("Group 2"), qsTr("Group 3")]
    }
    \endqml
*/
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
