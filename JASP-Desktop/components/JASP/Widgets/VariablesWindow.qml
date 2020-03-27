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

import QtQuick			2.7
import QtQuick.Controls 2.12 as New
import QtQuick.Controls 1.4 as OLD
import QtQuick.Layouts	1.3


FocusScope
{
	visible:						labelModel.visible

	property real calculatedMinimumHeight:	buttonColumnVariablesWindow.minimumHeight + columnNameVariablesWindow.height + 6 + (jaspTheme.generalAnchorMargin * 2)
	property bool blockEditing:				false

	Connections
	{
		target: labelModel
		
		onChosenColumnChanged:
		{
			blockEditing = true;
			if(labelModel.chosenColumn > -1 && labelModel.chosenColumn < dataSetModel.columnCount())
			{
				//to prevent the editText in the labelcolumn to get stuck and overwrite the next columns data... We have to remove activeFocus from it
				levelsTableViewRectangle.focus = true //So we just put it somewhere
				columnNameVariablesWindow.text = labelModel.columnName
				levelsTableView.selection.clear()
			}
			blockEditing = false;
		}
	}
	
	Rectangle
	{
		color:				jaspTheme.uiBackground
		border.color:		jaspTheme.uiBorder
		border.width:		1
		anchors.fill:		parent
	}

	Item
	{
		id:					levelsTableViewRectangle
		anchors.fill:		parent
		anchors.margins:	jaspTheme.generalAnchorMargin
		
		Text
		{
			id:				columnNameVariablesWindow
			text:			"Column Name/Title here"
			color:			jaspTheme.textEnabled
			font:			jaspTheme.fontGroupTitle
			anchors.top:	parent.top
			anchors.left:	parent.left
		}
		
		Item
		{
			anchors
			{
				top:			columnNameVariablesWindow.bottom
				left:			parent.left
				right:			parent.right
				bottom:			parent.bottom
				bottomMargin:	6
				topMargin:		6
			}

			TableViewJasp
			{
				id:				levelsTableView
				objectName:		"levelsTableView"
				anchors
				{
					top:			parent.top
					left:			parent.left
					right:			buttonColumnVariablesWindow.left
					bottom:			parent.bottom
					rightMargin:	2
				}
				
				model: labelModel
				
				selectionMode: OLD.SelectionMode.ExtendedSelection
				
				property var copiedSelection: []
				
				function copySelection()
				{
					copiedSelection = []
					selection.forEach( function(rowIndex) { copiedSelection.push(rowIndex) } )
				}
				
				function copySelectionReversed()
				{
					copiedSelection = []
					var nonreversedSelectionCopy = []
					selection.forEach( function(rowIndex) { nonreversedSelectionCopy.push(rowIndex) } )
					for(var i=nonreversedSelectionCopy.length - 1; i >= 0; i--)
						copiedSelection.push(nonreversedSelectionCopy[i])
				}
				
				function resizeValueColumn()
				{
					var title = "Values!"
					var minimumWidth = calculateMinimumRequiredColumnWidthTitle(1, title, 0, 0)
					levelsTableViewValueColumn.width = minimumWidth + 10
				}
				
				function moveUp()
				{
					levelsTableViewRectangle.focus = true
					copySelection()
					if(copiedSelection.length > 0 && copiedSelection[0] !== 0)
					{
						labelModel.moveUpFromQML(copiedSelection)
						
						selection.clear()
						
						for(var i=0; i<copiedSelection.length; i++)
						{
							var selectThis = copiedSelection[i]
							if(selectThis > 0)
								selection.select(selectThis - 1, selectThis - 1)
						}
					}
				}
				
				function moveDown()
				{
					levelsTableViewRectangle.focus = true
					copySelectionReversed()
					if(copiedSelection.length > 0 && (copiedSelection[0] != (labelModel.rowCount() - 1)))
					{
						labelModel.moveDownFromQML(copiedSelection)
						
						selection.clear()
						
						for(var i=0; i<copiedSelection.length; i++)
						{
							var selectThis = copiedSelection[i]
							
							if(selectThis < labelModel.rowCount() - 1)
								selection.select(selectThis + 1, selectThis + 1)
						}
					}
				}
				
				function reverse()
				{
					levelsTableViewRectangle.focus = true
					copySelection()
					labelModel.reverse()
					selection.clear()
					var maxSelect = labelModel.rowCount() - 1
					
					for(var i=0; i<copiedSelection.length; i++)
					{
						var selectThis = maxSelect - copiedSelection[i]
						selection.select(selectThis, selectThis)
					}
				}
				
				function closeYourself() { labelModel.visible = false; }
				
				
				OLD.TableViewColumn	{ id: levelsTableViewFilterColumn;	title: qsTr("Filter");	role: "filter";		width: 60 * preferencesModel.uiScale;				}
				OLD.TableViewColumn { id: levelsTableViewValueColumn;	title: qsTr("Value");	role: "value";		width: 120 * preferencesModel.uiScale;				}
				OLD.TableViewColumn { id: levelsTableViewLabelColumn;	title: qsTr("Label");	role: "display";	width:
						levelsTableView.width - levelsTableViewValueColumn.width - (20 * preferencesModel.uiScale) - levelsTableViewFilterColumn.width;					}
				
				headerDelegate: Rectangle
				{
					//Two rectangles to show a border of exactly 1px around cells
					id:				headerBorderRectangleVars
					color:			jaspTheme.grayDarker
					border.width:	0
					radius:			0
					height:			headerTextVars.contentHeight + (jaspTheme.itemPadding * 2)
					//width: headerTextVars.width + 8
					
					Rectangle
					{
						id:		colHeaderVars
						color:	jaspTheme.uiBackground
						
						x:		headerBorderRectangleVars.x
						y:		headerBorderRectangleVars.y
						height: headerBorderRectangleVars.height - 1
						width:	headerBorderRectangleVars.width - 1
						
						Text
						{
							id:		headerTextVars
							text:	styleData.value
							color:	jaspTheme.textEnabled
							font:	jaspTheme.font
							x:		jaspTheme.itemPadding
							
							anchors.verticalCenter: parent.verticalCenter
						}
					}
				}
				
				rowDelegate: Item { height: (30 * preferencesModel.uiScale)  }
				
				itemDelegate: Rectangle
				{
					color:			levelsTableView.selection.timesUpdated, levelsTableView.selection.contains(styleData.row) ? jaspTheme.itemHighlight : "transparent"
					
					New.Button
					{
						id: filterCheckButton
						checkable: true
						visible: styleData.column === 0
						
						anchors
						{
							top:				parent.top
							bottom:				parent.bottom
							horizontalCenter:	parent.horizontalCenter
							margins:			4
						}
						
						width: height
						
						checked: styleData.value
						
						
						onClicked:
						{
							labelModel.setData(labelModel.index(styleData.row, styleData.column), checked);
							checked = Qt.binding(function(){ return styleData.value; });
						}

						
						background: Image
						{
							source:				filterCheckButton.checked ? jaspTheme.iconPath + "/check-mark.png" : jaspTheme.iconPath + "cross.png"
							sourceSize.width:	Math.max(40, width)
							sourceSize.height:	Math.max(40, height)
							width:				filterCheckButton.width
							height:				filterCheckButton.height
							
						}
						
					}
					
					Text
					{
						visible:			styleData.column === 1
						
						color:				jaspTheme.grayDarker
						text:				styleData.value
						elide:				Text.ElideMiddle
						font:				jaspTheme.font
						anchors.fill:		parent
						verticalAlignment:	Text.AlignVCenter
					}
					
					TextInput
					{
						visible:		styleData.column === 2
						
						color:			jaspTheme.textEnabled
						
						text:			styleData.value
						font:			jaspTheme.font
						clip:			true
						selectByMouse:	true
						autoScroll:		true
						
						anchors.fill:	parent
						verticalAlignment: Text.AlignVCenter
						
						function acceptChanges()
						{
							if(!blockEditing && styleData.row >= 0 && styleData.column >= 0)
								labelModel.setData(labelModel.index(styleData.row, styleData.column), text)
						}
						onEditingFinished: focus = false
						
						onActiveFocusChanged:
							if(activeFocus)
							{
								levelsTableView.selection.clear()
								levelsTableView.selection.select(styleData.row, styleData.row)
							}
							else
							{
								if(focus)
									focus = false
								acceptChanges()
							}
						
						
						MouseArea
						{
							anchors.fill:		parent
							acceptedButtons:	Qt.NoButton
							cursorShape:		Qt.IBeamCursor
						}
					}
				}
			}
			
			ColumnLayout
			{
				id:					buttonColumnVariablesWindow
				
				anchors.top:		parent.top
				anchors.right:		parent.right
				anchors.bottom:		parent.bottom
				spacing:			Math.max(1, 2 * preferencesModel.uiScale)
				property int	shownButtons:		4 + (eraseFiltersOnThisColumn.visible ? 1 : 0) + (eraseFiltersOnAllColumns.visible ? 1 : 0)
				property real	minimumHeight:		(buttonHeight + spacing) * shownButtons + (3 * spacing)
				property real	buttonHeight:		32 * preferencesModel.uiScale
				
				RectangularButton
				{
					//text: "UP"
					iconSource:		jaspTheme.iconPath + "arrow-up.png"
					
					onClicked:		levelsTableView.moveUp()
					toolTip:		qsTr("Move selected labels up")
					
					height:			buttonColumnVariablesWindow.buttonHeight
					implicitHeight: buttonColumnVariablesWindow.buttonHeight
					width:			height
				}
				
				RectangularButton
				{
					//text: "DOWN"
					iconSource:		jaspTheme.iconPath + "arrow-down.png"
					
					onClicked:		levelsTableView.moveDown()
					toolTip:		qsTr("Move selected labels down")
					
					height:			buttonColumnVariablesWindow.buttonHeight
					implicitHeight: buttonColumnVariablesWindow.buttonHeight
					width:			height
				}
				
				RectangularButton
				{
					//text: "REVERSE"
					iconSource:		jaspTheme.iconPath + "arrow-reverse.png"
					onClicked:		levelsTableView.reverse()
					
					toolTip:		qsTr("Reverse order of all labels")
					
					height:			buttonColumnVariablesWindow.buttonHeight
					implicitHeight: buttonColumnVariablesWindow.buttonHeight
					width:			height
				}
				
				RectangularButton
				{
					id:				eraseFiltersOnThisColumn
					iconSource:		jaspTheme.iconPath + "eraser.png"
					onClicked:		labelModel.resetFilterAllows()
					visible:		labelModel.filteredOut > 0
					
					toolTip:		qsTr("Reset all filter checkmarks for this column")
					
					height:			buttonColumnVariablesWindow.buttonHeight
					implicitHeight: buttonColumnVariablesWindow.buttonHeight
					width:			height
				}
				
				RectangularButton
				{
					id:				eraseFiltersOnAllColumns
					iconSource:		jaspTheme.iconPath + "eraser_all.png"
					onClicked:		dataSetModel.resetAllFilters()
					visible:		dataSetModel.columnsFilteredCount > (labelModel.filteredOut > 0 ? 1 : 0)
					height:			buttonColumnVariablesWindow.buttonHeight
					implicitHeight: buttonColumnVariablesWindow.buttonHeight
					width:			height
					
					toolTip:		qsTr("Reset all filter checkmarks for all columns")
				}
				
				Item //Spacer
				{
					Layout.fillHeight: true
				}
				
				RectangularButton
				{
					id:				variablesWindowCloseButton
					iconSource:		jaspTheme.iconPath + "cross.png"
					onClicked:		labelModel.visible = false;
					height:			buttonColumnVariablesWindow.buttonHeight
					implicitHeight: buttonColumnVariablesWindow.buttonHeight
					width:			height
					
					toolTip: qsTr("Close this view")
				}
			}
		}
		
	}
	
}
