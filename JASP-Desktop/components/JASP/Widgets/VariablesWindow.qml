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
import JASP				1.0
import JASP.Widgets		1.0
import JASP.Controls	1.0
import QtQuick.Controls 2.15
import QtQuick.Layouts	1.15
import "."

FocusScope
{
	id:			variablesContainer
	visible:	labelModel.visible

	property real calculatedMinimumHeight:	buttonColumnVariablesWindow.minimumHeight + columnNameVariablesWindow.height + 10 + (jaspTheme.generalAnchorMargin * 2)

	Connections
	{
		target: labelModel
		
		onChosenColumnChanged:
			if(labelModel.chosenColumn > -1 && labelModel.chosenColumn < dataSetModel.columnCount())
				//to prevent the editText in the labelcolumn to get stuck and overwrite the next columns data... We have to remove activeFocus from it
				levelsTableViewRectangle.focus = true //So we just put it somewhere
	}

	Item
	{
		id:		minWidthVariables

		property int minWidth: 500 * preferencesModel.uiScale

		anchors
		{
			fill:			parent
			rightMargin:	Math.min(0, variablesContainer.width - minWidth)
		}

		Rectangle
		{
			color:				jaspTheme.uiBackground
			border.color:		jaspTheme.uiBorder
			border.width:		1
			anchors.fill:		parent
			z:					-1
		}

		Item
		{
			id:					levelsTableViewRectangle
			anchors.fill:		parent
			anchors.margins:	jaspTheme.generalAnchorMargin

			Text
			{
				id:					columnNameVariablesWindow
				text:				labelModel.columnName
				color:				jaspTheme.textEnabled
				font:				jaspTheme.fontGroupTitle
				anchors
				{
					horizontalCenter:	parent.horizontalCenter
					top:				parent.top
					topMargin:			jaspTheme.generalAnchorMargin
				}
			}

			Rectangle
			{
				id:					tableBackground
				color:				jaspTheme.controlBackgroundColor
				border.color:		jaspTheme.uiBorder
				border.width:		1

				anchors
				{
					top:			columnNameVariablesWindow.bottom
					left:			parent.left
					right:			buttonColumnVariablesWindow.left
					bottom:			parent.bottom
					margins:		jaspTheme.generalAnchorMargin
				}


				JASPDataView
				{
					id:				levelsTableView
					anchors
					{
						top:			parent.top
						left:			parent.left
						right:			parent.right
						bottom:			parent.bottom
					}

					model:					labelModel
					//cacheItems:				false //messes up updating of the table on mac somehow
					mouseAreaEnabled:		false
					toolTip:				""

					property real filterColWidth:	60  * jaspTheme.uiScale
					property real valueColWidth:	120 * jaspTheme.uiScale
					property real selectColWidth:	60  * jaspTheme.uiScale
					property real labelColWidth:	levelsTableView.flickableWidth - (filterColWidth + valueColWidth + selectColWidth)

					Binding	{ target: labelModel; property: "filterColWidth";	value: levelsTableView.filterColWidth; }
					Binding	{ target: labelModel; property: "valueColWidth";	value: levelsTableView.valueColWidth;  }
					Binding	{ target: labelModel; property: "labelColWidth";	value: levelsTableView.labelColWidth;  }
					Binding	{ target: labelModel; property: "selectColWidth";	value: levelsTableView.selectColWidth;  }

					columnHeaderDelegate:	Item
						{
							z: -4
							Rectangle
							{
								color:					jaspTheme.uiBackground
								anchors.fill:			parent
								anchors.rightMargin:	1 
	
								Text
								{
									id:						headerTextVars
									text:					headerText
									font:					jaspTheme.font
									color:					jaspTheme.textEnabled
									anchors
									{
										left:			parent.left
										leftMargin:		jaspTheme.generalAnchorMargin
										verticalCenter:	parent.verticalCenter
									}
								}
							}
					}

					rowNumberDelegate:	Item { width: 0; height: 0; }

					itemDelegate: Item
					{
						z:	-4


						Rectangle
						{
							color:			itemSelected ? jaspTheme.itemHighlight : "transparent"
							anchors
							{
								fill:			parent
								topMargin:		-levelsTableView.itemVerticalPadding
								leftMargin:		-levelsTableView.itemHorizontalPadding
								rightMargin:	-levelsTableView.itemHorizontalPadding
								bottomMargin:	-levelsTableView.itemVerticalPadding
							}
							z:	-10
						}

						Button
						{
							id:				filterCheckButton
							checkable:		true
							visible:		columnIndex === LabelModel.Filter
							checked:		itemText === "true"
							anchors.fill:	parent

							onClicked:
							{
								labelModel.setData(labelModel.index(rowIndex, columnIndex), checked, -1);
								checked = Qt.binding(function(){ return itemText === "true" ; });
							}

							background: Item
							{
								Image
								{
									source:					filterCheckButton.checked ? jaspTheme.iconPath + "check-mark.png" : jaspTheme.iconPath + "cross.png"
									sourceSize.width:		Math.max(40, width)
									sourceSize.height:		Math.max(40, height)
									//height:					filterCheckButton.height
									width:					height
									anchors
									{
										top:				parent.top
										bottom:				parent.bottom
										horizontalCenter:	parent.horizontalCenter
									}
								}
							}

							MouseArea
							{
								anchors.fill:		parent
								acceptedButtons:	Qt.NoButton
								cursorShape:		Qt.PointingHandCursor
							}

						}

						Text
						{
							visible:			columnIndex === LabelModel.Value

							color:				jaspTheme.grayDarker
							text:				itemText
							elide:				Text.ElideMiddle
							font:				jaspTheme.font
							anchors.fill:		parent
							verticalAlignment:	Text.AlignVCenter
						}

						TextInput
						{
							visible:		columnIndex === LabelModel.Label

							color:			jaspTheme.textEnabled

							text:			itemText
							font:			jaspTheme.font
							clip:			true
							selectByMouse:	true
							autoScroll:		true

							anchors.fill:	parent
							verticalAlignment: Text.AlignVCenter

							property int chosenColumnWas: -1

							function acceptChanges()
							{
								if(chosenColumnWas === labelModel.chosenColumn && rowIndex >= 0 && columnIndex >= 0)
									labelModel.setData(labelModel.index(rowIndex, columnIndex), text, -1)
							}
							onEditingFinished: focus = false

							onActiveFocusChanged:
								if(activeFocus)
								{
									chosenColumnWas = labelModel.chosenColumn
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

						Button
						{
							id:				selectionButton
							checkable:		true
							visible:		columnIndex === LabelModel.Selection
							checked:		itemSelected
							anchors.fill:	parent

							onClicked:
							{
								var wasChecked = checked;
								checked = Qt.binding(function(){ return itemSelected ; });
								labelModel.setData(labelModel.index(rowIndex, columnIndex), wasChecked, -1);
							}

							background: Item
							{

								Rectangle
								{
									border.color:		jaspTheme.uiBorder
									border.width:		2 * jaspTheme.uiScale
									color:				selectionButton.checked ? jaspTheme.jaspBlue : "transparent"
									//radius:				width
									width:				height
									anchors
									{
										top:				parent.top
										bottom:				parent.bottom
										horizontalCenter:	parent.horizontalCenter
									}
								}
							}

							MouseArea
							{
								anchors.fill:		parent
								acceptedButtons:	Qt.NoButton
								cursorShape:		Qt.PointingHandCursor
							}

						}
					}
				}
			}

			ColumnLayout
			{
				id:					buttonColumnVariablesWindow

				anchors.top:		tableBackground.top
				anchors.right:		parent.right
				anchors.bottom:		tableBackground.bottom
				spacing:			Math.max(1, 2 * preferencesModel.uiScale)

				property int	shownButtons:		4 + (eraseFiltersOnThisColumn.visible ? 1 : 0) + (eraseFiltersOnAllColumns.visible ? 1 : 0)
				property real	minimumHeight:		(buttonHeight + spacing) * shownButtons + (3 * spacing)
				property real	buttonHeight:		32 * preferencesModel.uiScale

				RectangularButton
				{
					//text: "UP"
					iconSource:		jaspTheme.iconPath + "arrow-up.png"

					onClicked:		labelModel.moveSelectionUp()
					toolTip:		qsTr("Move selected labels up")

					height:			buttonColumnVariablesWindow.buttonHeight
					implicitHeight: buttonColumnVariablesWindow.buttonHeight
					width:			height
				}

				RectangularButton
				{
					//text: "DOWN"
					iconSource:		jaspTheme.iconPath + "arrow-down.png"

					onClicked:		labelModel.moveSelectionDown()
					toolTip:		qsTr("Move selected labels down")

					height:			buttonColumnVariablesWindow.buttonHeight
					implicitHeight: buttonColumnVariablesWindow.buttonHeight
					width:			height
				}

				RectangularButton
				{
					//text: "REVERSE"
					iconSource:		jaspTheme.iconPath + "arrow-reverse.png"
					onClicked:		labelModel.reverse()

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
