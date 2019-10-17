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


import QtQuick			2.11
import QtQuick.Controls	2.5
import QtQuick.Controls	2.5 as QTC
import QtQuick.Layouts	1.3
import JASP.Controls	1.0
import JASP.Theme		1.0
import QtQuick.Window	2.3
import JASP				1.0

JASPControl
{
	id:					tableView

	controlType:		"TableView"
	activeFocusOnTab:	false
	width:				implicitWidth
	height:				implicitHeight
	implicitWidth:		400
	implicitHeight:		400

	property var	source
	property alias	syncModels:		tableView.source
	property string	modelType
	property string	itemType:		"string"
	property string filter:			"rep(TRUE, rowcount)"	//Used by ListModelFilteredDataEntry
	property string colName:		"data"					//Used by ListModelFilteredDataEntry
	property string	extraCol:		""						//Used by ListModelFilteredDataEntry
	property string	tableType
	property alias	model:			theView.model
	property var	validator:		(itemType === "integer") ? intValidator : (itemType === "double" ? doubleValidator : stringValidator)
	property int	colSelected:	-1
	property int	rowSelected:	-1
	property int	columnCount:	0	//Readonly
	property int	rowCount:		0	//Readonly

	property int	initialColumnCount:		0	//Only read on init
	property int	initialRowCount:		0	//Only read on init


	signal reset()
	signal addRow()
	signal addColumn()
	signal removeRow(int row)
	signal removeColumn(int col)
	signal itemChanged(int col, int row, string value)

	//These signals are added because I had some trouble connecting the filterChanged from C++ (in constructor of ListModelFilteredDataEntry)
	signal filterSignal(string filter)
	signal colNameSignal(string filter)
	signal extraColSignal(string extraCol)

	onFilterChanged:	filterSignal(tableView.filter)
	onColNameChanged:	colNameSignal(tableView.colName)
	onExtraColChanged:	extraColSignal(tableView.extraCol)

	function removeAColumn()
	{
		if (colSelected >= 0)
			removeColumn(colSelected);
	}

	function removeARow()
	{
		if (rowSelected >= 0)	removeRow(rowSelected);
		else					removeRow(rowCount - 1);
	}

	Rectangle
	{
		id:					rectangleBorder
		anchors.centerIn:	parent
		width:				parent.width
		height:				parent.height
		border.width:		1
		border.color:		Theme.borderColor
		color:				Theme.white
	}

	Flickable
	{
		id:				myFlickable
		anchors.top:	parent.top
		anchors.left:	parent.left
		anchors.right:	vertiScroller.left
		anchors.bottom: horiScroller.top
		contentWidth:	theView.width
		contentHeight:	theView.height
		clip:			true
		boundsBehavior	: Flickable.StopAtBounds
		boundsMovement	: Flickable.StopAtBounds

		DataSetView
		{
			z:						-1
			id:						theView
			model:					null
			itemHorizontalPadding:	0
			itemVerticalPadding:	8 * preferencesModel.uiScale

			viewportX: myFlickable.visibleArea.xPosition * width
			viewportY: myFlickable.visibleArea.yPosition * height
			viewportW: myFlickable.visibleArea.widthRatio * width
			viewportH: myFlickable.visibleArea.heightRatio * height

			columnHeaderDelegate: Rectangle
			{
				color: columnIndex === tableView.colSelected ? Theme.grayLighter : Theme.analysisBackgroundColor
				Text { text: headerText; anchors.centerIn: parent; font: Theme.font }
				MouseArea
				{
					anchors.fill: parent
					onClicked: {
						if (tableView.colSelected === columnIndex)
							columnIndex = -1
						tableView.colSelected = columnIndex;
					}
				}
			}

			rowNumberDelegate: Rectangle
			{
				color: Theme.analysisBackgroundColor
				Text
				{
					text:					headerText;
					anchors.centerIn:		parent;
					horizontalAlignment:	Text.AlignHCenter
					verticalAlignment:		Text.AlignVCenter
					leftPadding:			3 * preferencesModel.uiScale
					elide:					Text.ElideRight;
					width:					parent.width
					height:					parent.width
					font:					Theme.font
				}
			}

			JASPDoubleValidator	{ id: intValidator;		bottom: 0; decimals: 0	}
			JASPDoubleValidator { id: doubleValidator;	bottom: 0; decimals: 1	}
			RegExpValidator		{ id: stringValidator							}

			itemDelegate: Rectangle
			{
				Text
				{
					id:					textDisplay
					anchors.fill:	 	parent
					font:				Theme.font
					color:				itemEditable ? Theme.textEnabled : Theme.textDisabled
					visible:			!textInput.visible
					text:				itemText
					padding:			Theme.jaspControlPadding
					leftPadding:		Theme.labelSpacing
					verticalAlignment:	Text.AlignVCenter
				}

				MouseArea
				{
					anchors.fill:		parent
					visible:			itemEditable && !textInput.visible
					z:					2
					onClicked:
					{
						textInput.visible	= true;
						textInput.text		= itemText === "..." ? "" : itemText
						textInput.forceActiveFocus();
					}
					cursorShape:		Qt.IBeamCursor
				}

				QTC.TextField
				{
					id:					textInput
					anchors.fill:		parent
					visible:			false
					text:				""
					font:				Theme.font
					leftPadding:		Theme.labelSpacing
					padding:			Theme.jaspControlPadding
					verticalAlignment:	Text.AlignVCenter
					selectByMouse:		true
					selectedTextColor:	Theme.white
					selectionColor:		Theme.itemSelectedColor
					validator:			tableView.validator
					onPressed:			tableView.colSelected = columnIndex
					onEditingFinished:
					{
						tableView.itemChanged(columnIndex, rowIndex, text)
						focus = false;
					}
					onActiveFocusChanged: if(!activeFocus) visible = false;

				}
			}

			leftTopCornerItem: Rectangle
			{
				color: Theme.analysisBackgroundColor

				Text
				{
					text:					"Row #"
					horizontalAlignment:	Text.AlignHCenter
					verticalAlignment:		Text.AlignVCenter
					leftPadding:			3 * preferencesModel.uiScale
					elide:					Text.ElideRight;
					width:					parent.width
					height:					parent.height
					font:					Theme.font
					anchors.right:			parent.right
					anchors.bottom:			parent.bottom
				}
			}

		}
	}

	JASPScrollBar
	{
		id:				vertiScroller;
		flickable:		myFlickable
		anchors.top:	parent.top
		anchors.right:	parent.right
		anchors.bottom: horiScroller.top
		bigBar:			false
	}

	JASPScrollBar
	{
		id:				horiScroller;
		flickable:		myFlickable
		vertical:		false
		anchors.left:	parent.left
		anchors.right:	vertiScroller.left
		anchors.bottom: parent.bottom
		bigBar:			false
	}
}
