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


import QtQuick			2.15
import QtQuick.Controls	2.12 as QTC
import QtQuick.Layouts	1.3
import JASP.Controls	1.0
import QtQuick.Window	2.3
import JASP				1.0

TableViewBase
{
	id:					tableView
	focusOnTab:			false
	implicitWidth:		400
	implicitHeight:		400
	shouldStealHover:	false
	defaultValue:		modelType === JASP.JAGSDataInputModel	? "..." : (modelType === JASP.CustomContrasts	? "0" : "1")
	initialColumnCount: modelType === JASP.MultinomialChi2Model ? 1 : 0

	property string factorsSource	: ""
	property string filter			: "rep(TRUE, rowcount)"	//Used by ListModelFilteredDataEntry
	property string colName			: modelType === JASP.CustomContrasts ? "" : "data"					//Used by ListModelFilteredDataEntry
	property string	extraCol		: ""						//Used by ListModelFilteredDataEntry
	property alias	 rowNumberWidth	: theView.rowNumberWidth
	property var	validator		: (itemType === JASP.Integer) ? intValidator : (itemType === JASP.Double ? doubleValidator : stringValidator)
	property double	minimum			: 0
	property int	decimals		: 1
	property int	colSelected		: -1
	property int	rowSelected		: -1
	property real	scaleFactor		: 1 // Only used by marginalMeansContrast
	property string cornerText		: qsTr("Row #")
	property bool	parseDefaultValue	: true
	property bool	isFirstColEditable	: true
	property bool	showAddRemoveButtons: modelType === JASP.GridInput

	property alias  view					: theView
	property alias  itemDelegate			: theView.itemDelegate
	property alias  rowNumberDelegate		: theView.rowNumberDelegate
	property alias  columnHeaderDelegate	: theView.columnHeaderDelegate
	property alias  leftTopCornerItem		: theView.leftTopCornerItem

	property alias	addLeftButton	: addLeftButton
	property alias	addRightButton	: addRightButton
	property alias	deleteButton	: deleteButton

	//The size of the table *inside* the Flickable. + 2 for margins of flickable and scrollbars
	readonly property int tableWidth:  theView.width  + 2 + (vertiScroller.visible ? jaspTheme.scrollbarBoxWidth : 0)
	readonly property int tableHeight: theView.height + 2 + (horiScroller.visible ? jaspTheme.scrollbarBoxWidth : 0)

	// These 4 functions can be overloaded to set a custom column or row header, or a default value, or a default validator.
	function getColHeaderText(headerText, columnIndex)			{ return (columnNames.length > columnIndex)	? columnNames[columnIndex]	: headerText; }
	function getRowHeaderText(headerText, rowIndex)				{ return (rowNames.length > rowIndex)		? rowNames[rowIndex]		: headerText; }
	function getDefaultValue(columnIndex, rowIndex)				{ return defaultValue;	}
	function getValidator(columnIndex, rowIndex)				{ return validator;	}

	//These signals are added because I had some trouble connecting the filterChanged from C++ (in constructor of ListModelFilteredDataEntry)
	signal filterSignal(string filter)
	signal colNameSignal(string filter)
	signal extraColSignal(string extraCol)

	onFilterChanged:	filterSignal(tableView.filter)
	onColNameChanged:	colNameSignal(tableView.colName)
	onExtraColChanged:	extraColSignal(tableView.extraCol)

	property real iconSize:			12 * preferencesModel.uiScale
	property real iconSizeHovered:	14 * preferencesModel.uiScale

	onColSelectedChanged: setButtons()

	function setButtons()
	{
		if (!showAddRemoveButtons) return

		var item
		if (colSelected >= 0) item = theView.getColumnHeader(colSelected);

		if (item)
		{
			if (maxColumn < 0 || maxColumn > model.columnCount())
			{
				var maxNumberWidth = theView.rowNumberWidth;
				addLeftButton.x = Qt.binding(function() { return 1 + item.x - iconSize/2 - myFlickable.contentX } )
				addLeftButton.y = Qt.binding(function() { return item.y + item.height/2 - iconSize/2 } )
				addLeftButton.visible = Qt.binding(function() { return (addLeftButton.x + iconSize/2 > theView.rowNumberWidth) && (addLeftButton.x + iconSize/2 < tableView.width + 1) } )
				addRightButton.x = Qt.binding(function() { return 1 + item.x + item.width - iconSize/2 - myFlickable.contentX } )
				addRightButton.y = Qt.binding(function() { return item.y + item.height/2 - iconSize/2 } )
				addRightButton.visible = Qt.binding(function() { return (addRightButton.x + iconSize/2 > theView.rowNumberWidth) && (addRightButton.x + iconSize/2 < tableView.width + 1) } )
			}
			else
			{
				addLeftButton.visible = false
				addRightButton.visible = false;
			}
			if (minColumn < model.columnCount())
			{
				deleteButton.x = Qt.binding(function() { return item.x + item.width/2 - iconSize/2 - myFlickable.contentX } )
				deleteButton.visible = Qt.binding(function() { return (deleteButton.x + iconSize/2 > theView.rowNumberWidth ) && (deleteButton.x + iconSize/2 < tableView.width + 1) } )
			}
			else
				deleteButton.visible = false
		}
		else
		{
			addLeftButton.visible = false
			addRightButton.visible = false
			deleteButton.visible = false
		}
	}

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
		border.color:		jaspTheme.uiBorder
		color:				jaspTheme.white

		MenuButton
		{
			id:					addLeftButton
			z:					100
			anchors.top:		parent.top
			anchors.topMargin:	-height/2
			height:				hovered ? iconSizeHovered : iconSize
			width:				height
			buttonPadding:		0
			color:				"transparent"
			iconSource:			jaspTheme.iconPath + "/addition-sign-small-green.svg"
			toolTip:			qsTr("Add to the left")
			radius:				height
			visible:			false
			onClicked:
			{
				if (maxColumn < 0 || maxColumn > model.columnCount())
				{
					addColumn(colSelected, true);
					colSelected++;
				}
			}
		}

		MenuButton
		{
			id:					addRightButton
			z:					100
			anchors.top:		parent.top
			anchors.topMargin:	-height/2
			height:				hovered ? iconSizeHovered : iconSize
			width:				height
			buttonPadding:		0
			color:				"transparent"
			iconSource:			jaspTheme.iconPath + "/addition-sign-small-green.svg"
			toolTip:			qsTr("Add to the right")
			radius:				height
			visible:			false
			onClicked:
			{
				if (maxColumn < 0 || maxColumn > model.columnCount())
				{
					addColumn(colSelected, false)
					setButtons()
				}
			}
		}

		MenuButton
		{
			id:					deleteButton
			z:					100
			anchors.top:		parent.top
			anchors.topMargin:	-height/2
			height:				(hovered ? iconSizeHovered : iconSize) - 1
			width:				height
			buttonPadding:		0
			color:				"transparent"
			iconSource:			jaspTheme.iconPath + "/cross-sign-small-red.svg"
			toolTip:			qsTr("Delete")
			radius:				height
			visible:			false
			onClicked:
			{
				if (minColumn < model.columnCount())
				{
					removeColumn(colSelected);
					if (colSelected > 0) colSelected--
					else setButtons()
				}
			}
		}


		Flickable
		{
			id:				myFlickable
			anchors
			{
				topMargin:	1
				leftMargin:	1
				top:		parent.top
				left:		parent.left
				right:		vertiScroller.left
				bottom:		horiScroller.top
			}

			contentWidth:	theView.width
			contentHeight:	theView.height

			boundsBehavior	: Flickable.StopAtBounds
			boundsMovement	: Flickable.StopAtBounds
			clip:			true

			DataSetView
			{
				z:						-1
				id:						theView
				model:					tableView.model
				itemHorizontalPadding:	0
				itemVerticalPadding:	8 * preferencesModel.uiScale
				cacheItems:				false
				tableViewItem:			tableView

				viewportX: myFlickable.visibleArea.xPosition * width
				viewportY: myFlickable.visibleArea.yPosition * height
				viewportW: myFlickable.visibleArea.widthRatio * width
				viewportH: myFlickable.visibleArea.heightRatio * height

				columnHeaderDelegate: Rectangle
				{
					color: columnIndex === tableView.colSelected ? jaspTheme.grayLighter : jaspTheme.analysisBackgroundColor
					Text { text: tableView.getColHeaderText(headerText, columnIndex); anchors.centerIn: parent; font: jaspTheme.font; color:	jaspTheme.textEnabled }
					MouseArea
					{
						anchors.fill: parent
						onClicked: 
						{
							if (tableView.colSelected === columnIndex)
								columnIndex = -1
							tableView.colSelected = columnIndex;
						}
					}
				}

				rowNumberDelegate: Rectangle
				{
					color: rowIndex === tableView.rowSelected ? jaspTheme.grayLighter : jaspTheme.analysisBackgroundColor
					Text
					{
						text:					tableView.getRowHeaderText(headerText, rowIndex);
						color:					jaspTheme.textEnabled
						anchors.centerIn:		parent;
						horizontalAlignment:	Text.AlignHCenter
						verticalAlignment:		Text.AlignVCenter
						leftPadding:			3 * preferencesModel.uiScale
						elide:					Text.ElideRight;
						width:					parent.width
						height:					parent.width
						font:					jaspTheme.font
					}
					
					MouseArea
					{
						anchors.fill: parent
						onClicked: 
						{
							if (tableView.rowSelected === rowIndex)
								rowIndex = -1
							tableView.rowSelected = rowIndex;
						}
					}
				}

				JASPDoubleValidator			{ id: intValidator;		bottom: tableView.minimum; decimals: 0					}
				JASPDoubleValidator			{ id: doubleValidator;	bottom: tableView.minimum; decimals: tableView.decimals	}
				RegularExpressionValidator	{ id: stringValidator	}

				itemDelegate: Item
				{
					FormulaField
					{
						id:						formlaInput
						inputType:				itemInputType
						isBound:				false
						anchors.verticalCenter: parent.verticalCenter
						anchors.left:			parent.left
						anchors.top:			parent.top
						anchors.topMargin:		- 8 * preferencesModel.uiScale
						fieldHeight:			parent.height + 16 * preferencesModel.uiScale
						fieldWidth:				parent.width
						useExternalBorder:		false
						showBorder:				false
						value:					itemText
						useLastValidValue:		false
						parseDefaultValue:		tableView.parseDefaultValue
						defaultValue:			tableView.getDefaultValue(columnIndex, rowIndex)
						selectValueOnFocus:		true
						validator:				getValidator(columnIndex, rowIndex)
						onPressed:				tableView.colSelected = columnIndex
						onEditingFinished:
						{
							tableView.itemChanged(columnIndex, rowIndex, value, inputType)
							tableView.setButtons()
						}
						editable:				itemEditable
						multiple:				itemInputType === "formulaArray"
					}
				}

				leftTopCornerItem: Rectangle
				{
					color: jaspTheme.analysisBackgroundColor

					Text
					{
						text:					cornerText
						horizontalAlignment:	Text.AlignHCenter
						verticalAlignment:		Text.AlignVCenter
						color:					jaspTheme.textEnabled
						height:					parent.height
						font:					jaspTheme.font
						anchors.centerIn:		parent
					}
				}

			}
		}

		JASPScrollBar
		{
			id:				vertiScroller;
			flickable:		myFlickable
			manualAnchor:	true
			visible:		myFlickable.visible && tableView.height < theView.height
			anchors
			{
				top:			parent.top
				right:			parent.right
				bottom:			horiScroller.top
				topMargin:		1
				rightMargin:	1
			}
			bigBar:			false
		}

		JASPScrollBar
		{
			id:				horiScroller;
			flickable:		myFlickable
			vertical:		false
			manualAnchor:	true
			visible:		myFlickable.visible && tableView.width < theView.width
			anchors
			{
				left:			parent.left
				right:			vertiScroller.left
				bottom:			parent.bottom
				leftMargin:		1
				bottomMargin:	1
			}
			bigBar:			false
		}
	}
}
