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

import QtQuick.Window	2.3
import JASP				1.0

JASPControl
{
	id:					tableView

	controlType:		JASPControlBase.TableView
	focusOnTab:			false
	implicitWidth:		400
	implicitHeight:		400
	shouldStealHover:	false

	property var	source
	property string factorsSource	: ""
	property alias	syncModels		: tableView.source
	property string	modelType
	property string	itemType		: "string"
	property string filter			: "rep(TRUE, rowcount)"	//Used by ListModelFilteredDataEntry
	property string colName			: modelType == "CustomContrasts" ? "" : "data"					//Used by ListModelFilteredDataEntry and ListMOdelANOVACustomContrasts
	property string	extraCol		: ""						//Used by ListModelFilteredDataEntry
	property string	tableType
	property alias	model			: theView.model
	property alias	rowNumberWidth	: theView.rowNumberWidth
	property var	validator		: (itemType === "integer") ? intValidator : (itemType === "double" ? doubleValidator : stringValidator)
	property int	minimum			: 0
	property int	decimals		: 1
	property int	colSelected		: -1
	property int	rowSelected		: -1
	property int	columnCount		: 0	//Readonly
	property int	rowCount		: 0	//Readonly
	property int	variableCount	: 0 //Readonly only used by MarginalMeansContrasts
	property real	scaleFactor		: 1 // Only used by marginalMeansContrast
	property string cornerText		: qsTr("Row #")
	property bool	parseDefaultValue: true
	property string defaultEmptyValue:	""

	property int	initialColumnCount	: 0	//Only read on init
	property int	initialRowCount		: 0	//Only read on init

	//The size of the table *inside* the Flickable. + 2 for margins of flickable and scrollbars
	readonly property int tableWidth:  theView.width  + 2
	readonly property int tableHeight: theView.height + 2

	signal reset()
	signal addRow()
	signal addColumn()
	signal removeRow(int row)
	signal removeColumn(int col)
	signal itemChanged(int col, int row, string value, string type)

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
		border.color:		jaspTheme.uiBorder
		color:				jaspTheme.white

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
			clip:				true

			DataSetView
			{
				z:						-1
				id:						theView
				model:					null
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
					Text { text: headerText; anchors.centerIn: parent; font: jaspTheme.font; color:	jaspTheme.textEnabled }
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
					color: jaspTheme.analysisBackgroundColor
					Text
					{
						text:					headerText;
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
				}

				JASPDoubleValidator	{ id: intValidator;		bottom: tableView.minimum; decimals: 0	}
				JASPDoubleValidator { id: doubleValidator;	bottom: tableView.minimum; decimals: tableView.decimals	}
				RegExpValidator		{ id: stringValidator							}

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
						defaultEmptyValue:		tableView.defaultEmptyValue
						selectValueOnFocus:		true
						validator:				tableView.validator
						onPressed:				tableView.colSelected = columnIndex
						onEditingFinished:		tableView.itemChanged(columnIndex, rowIndex, value, inputType)
						editable:				itemEditable
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
