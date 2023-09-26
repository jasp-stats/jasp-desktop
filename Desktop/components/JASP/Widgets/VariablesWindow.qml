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
import JASP
import JASP.Widgets
import JASP.Controls
import QtQuick.Controls as QTC
import QtQuick.Layouts
import "."


FocusScope
{
	id:			variablesContainer
	visible:	columnModel.visible

	property real calculatedBaseHeight:			common.height + jaspTheme.generalAnchorMargin
	property real calculatedMinimumHeight:		calculatedBaseHeight + (tabView.visible ?  0.28 * parent.height : 0)
	property real calculatedPreferredHeight:	calculatedBaseHeight + (tabView.visible ?  0.32 * parent.height : 0)
	property real calculatedMaximumHeight:		!tabView.visible ? calculatedBaseHeight :  parent.height * 0.7

	Connections
	{
		target: columnModel

		function onBeforeChangingColumn(chosenColumn)
		{
			if (columnModel.visible && columnModel.chosenColumn >= 0)
			{
				if(columnModel.columnName !== columnNameVariablesWindow.value)				columnModel.columnName			= columnNameVariablesWindow.value
				if(columnModel.columnTitle !== columnTitleVariablesWindow.value)			columnModel.columnTitle			= columnTitleVariablesWindow.value
				if(columnModel.columnDescription !== columnDescriptionVariablesWindow.text) columnModel.columnDescription	= columnDescriptionVariablesWindow.text
				if(columnModel.computedType !== computedTypeVariableWindow.value)			columnModel.computedType		= computedTypeVariableWindow.value
				if(columnModel.currentColumnType !== columnTypeVariableWindow.value)		columnModel.currentColumnType	= columnTypeVariableWindow.value
			}
		}
		
		function onChosenColumnChanged(chosenColumn)
		{
			if(columnModel.chosenColumn > -1 && columnModel.chosenColumn < dataSetModel.columnCount())
				//to prevent the editText in the labelcolumn to get stuck and overwrite the next columns data... We have to remove activeFocus from it
				common.focus = true //So we just put it somewhere
		}
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
			id:					common
			anchors.top:		parent.top
			anchors.left:		parent.left
			anchors.leftMargin: 0
			anchors.margins:	jaspTheme.generalAnchorMargin
			width:			    parent.width
			height:				columnNameVariablesWindow.height + columnDescriptionVariablesWindow.height + computedTypeVariableWindow.height + 4 * jaspTheme.generalAnchorMargin

			Label
			{
				id: nameLabel
				text: qsTr("Name: ")
				width: descriptionLabel.contentWidth > nameLabel.contentWidth ? descriptionLabel.implicitWidth : nameLabel.implicitWidth
				anchors
				{
					top:			parent.top
					topMargin:		jaspTheme.generalAnchorMargin
					left:			parent.left
					leftMargin:		jaspTheme.generalAnchorMargin
				}
			}

			TextField
			{
				id:					columnNameVariablesWindow
				placeholderText:	qsTr("<Fill in the name of the column>")
				value:				columnModel.columnName
				onValueChanged:		if(columnModel.columnName !== value) columnModel.columnName = value
				undoModel:			columnModel
                editable:           columnModel.nameEditable
                
				anchors
				{
					top:			nameLabel.top
					left:			nameLabel.right
					leftMargin:		jaspTheme.generalAnchorMargin
				}
			}


			TextField
			{
				id:					columnTitleVariablesWindow
				label:				qsTr("Long name: ");
				placeholderText:	qsTr("<Fill in a more descriptive name of the column>")
				fieldWidth:			Math.min(closeButton.x - x - control.x, columnNameVariablesWindow.fieldWidth * 1.5)
				value:				columnModel.columnTitle
				onValueChanged:		if(columnModel.columnTitle !== value) columnModel.columnTitle = value
				undoModel:			columnModel

				anchors
				{
					left:			columnNameVariablesWindow.right
					leftMargin:		jaspTheme.generalAnchorMargin
					top:			columnNameVariablesWindow.top
				}
			}

			Label
			{
				id: descriptionLabel
				text: qsTr("Description: ")
				width: descriptionLabel.contentWidth > nameLabel.contentWidth ? descriptionLabel.implicitWidth : nameLabel.implicitWidth
				anchors
				{
					top:			columnTitleVariablesWindow.bottom
					topMargin:		jaspTheme.generalAnchorMargin
					left:			parent.left
					leftMargin:		jaspTheme.generalAnchorMargin
				}
			}

			TextArea
			{
				id:					columnDescriptionVariablesWindow
				anchors
				{
					top:			descriptionLabel.top
					left:			descriptionLabel.right
					leftMargin:		jaspTheme.generalAnchorMargin
					right:			parent.right
					rightMargin:	jaspTheme.generalAnchorMargin
				}
				height:				Math.max(columnNameVariablesWindow.height, Math.min(maxHeight, control.contentHeight + 20 * jaspTheme.uiScale))
				control.padding:	3 * jaspTheme.uiScale

				text:				columnModel.columnDescription
				onEditingFinished: 	if(columnModel.columnDescription !== text) columnModel.columnDescription = text
				applyScriptInfo:	""
				placeholderText:	"..."
				undoModel:			columnModel
				useTabAsSpaces:		false

				property int maxHeight:	100 * jaspTheme.uiScale
			}

			DropDown
			{
				id: columnTypeVariableWindow

				anchors
				{
					top:			columnDescriptionVariablesWindow.bottom
					topMargin:		jaspTheme.generalAnchorMargin
					left:			parent.left
					leftMargin:		jaspTheme.generalAnchorMargin
				}

				label:				qsTr("Column type: ")
				isBound:			false
				showVariableTypeIcon: true
				values:				columnModel.columnTypeValues
				currentValue:		columnModel.currentColumnType
				onValueChanged:		columnModel.currentColumnType = currentValue
			}

			DropDown
			{
				id: computedTypeVariableWindow
				anchors
				{
					top:			columnDescriptionVariablesWindow.bottom
					topMargin:		jaspTheme.generalAnchorMargin
					left:			columnTypeVariableWindow.right
					leftMargin:		jaspTheme.generalAnchorMargin
				}

				label:				qsTr("Computed type: ")
				values:				columnModel.computedTypeValues
				currentValue:		columnModel.computedType
				onValueChanged:		columnModel.computedType = currentValue
				enabled:			columnModel.computedTypeEditable
			}

			MenuButton
			{
				id:					closeButton
				height:				33 * jaspTheme.uiScale
				width:				height
				iconSource:			jaspTheme.iconPath + "close-button.png"
				onClicked:			{ computedColumnWindow.askIfChangedOrClose(); columnModel.visible = false }
				toolTip:			qsTr("Close variable window")
				radius:				height
				anchors
				{
					top:			parent.top
					right:			parent.right
					rightMargin:	jaspTheme.generalAnchorMargin
				}
			}
		}

		Rectangle
		{
			id: tabView

			visible:				columnModel.tabs.length > 0

			anchors.top:			common.bottom
			anchors.left:			parent.left
			anchors.right:			parent.right
			anchors.bottom:			parent.bottom
			anchors.margins:		jaspTheme.generalAnchorMargin

			color: jaspTheme.uiBackground

			property var	currentTabButton
			property real	currentTabX:		currentTabButton ? currentTabButton.mapToItem(tabView, 0, 0).x : 0
			property real	currentTabWidth:	currentTabButton ? currentTabButton.width : 0

			QTC.TabBar
			{
				id: tabbar
				contentHeight:	tabBarHeight + tabButtonRadius
				width:			parent.width
				background: Rectangle { color: jaspTheme.uiBackground }

				property real	tabBarHeight:		28 * preferencesModel.uiScale
				property real	tabButtonRadius:	5 * preferencesModel.uiScale

				Repeater
				{
					id:		tabButtonRepeater
					model:	columnModel.tabs.length

					QTC.TabButton
					{
						id:			tabButton
						height:		tabbar.height
						width:		labelText.implicitWidth + 20 * preferencesModel.uiScale

						onCheckedChanged: if (checked) tabView.currentTabButton = tabButton

						background: Rectangle
						{
							color:			checked ? jaspTheme.uiBackground : jaspTheme.grayLighter
							radius:			tabbar.tabButtonRadius
							border.width:	1
							border.color:	checked ? jaspTheme.uiBorder : jaspTheme.borderColor
						}

						contentItem: Text
						{
							// The bottom of buttons are hidden to remove their bottom line with the radius
							// So the text has to be moved higher from the horizontal middle line.
							id:					labelText
							topPadding:			-tabbar.tabButtonRadius * 3/4
							text:				columnModel.tabs[index].title
							font:				jaspTheme.font
							color:				jaspTheme.black
							horizontalAlignment: Text.AlignHCenter
							verticalAlignment:	Text.AlignVCenter
							opacity:			checked ? 1 : .6
						}

						MouseArea
						{
							anchors.fill	: parent
							cursorShape		: checked ? Qt.ArrowCursor : Qt.PointingHandCursor
							acceptedButtons	: Qt.NoButton
						}
					}
				}
			}

			Rectangle
			{
				// This hides the bottom border of the buttons (with their radius)
				id		: roundingHider
				width	: tabbar.contentWidth
				height	: tabbar.tabButtonRadius + 1
				anchors
				{
					left:		parent.left
					top:		parent.top
					topMargin:	tabbar.tabBarHeight
				}
				color: jaspTheme.uiBackground
				z: 1

				Rectangle
				{
					// The Tabbar removes the left border. Redraw it.
					anchors.left:	parent.left
					anchors.top:	parent.top
					anchors.bottom: parent.bottom
					width:			1
					color:			jaspTheme.uiBorder
				}
			}

			Rectangle
			{
				// Rectangle to draw the border under the tabbar
				id: borderView
				anchors
				{
					fill:		parent
					topMargin:	tabbar.tabBarHeight
				}
				z:				1
				border.width:	1
				border.color:	jaspTheme.uiBorder
				color:			"transparent"
			}

			Rectangle
			{
				// Hide the line onder the active tab
				z:				1
				height:			1
				width:			tabView.currentTabWidth - 2
				color:			jaspTheme.uiBackground
				x:				tabView.currentTabX + 1
				anchors.top:	borderView.top
			}

			StackLayout
			{
				id: stack
				property var componentIndex:
				{
					"computed": 0,
					"label" : 1
				}
				currentIndex: tabbar.currentIndex >= 0 ? componentIndex[columnModel.tabs[tabbar.currentIndex].name] : -1

				anchors.top: tabbar.bottom
				anchors.bottom: parent.bottom
				anchors.left:	parent.left
				anchors.right:	parent.right
				anchors.margins: jaspTheme.generalAnchorMargin * 0.5
				anchors.topMargin: jaspTheme.generalAnchorMargin * 0.25

				ComputeColumnWindow
				{
					id: computedColumnWindow
				}

				Rectangle
				{
					color:			jaspTheme.uiBackground

					Rectangle
					{
						id:					tableBackground
						color:				jaspTheme.controlBackgroundColor
						border.color:		jaspTheme.uiBorder
						border.width:		1

						anchors
						{
							top:			parent.top
							left:			parent.left
							right:			buttonColumnVariablesWindow.left
							bottom:			parent.bottom
							rightMargin:	jaspTheme.generalAnchorMargin
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

							model:						columnModel
							cacheItems:					false
							expandDataSet:				false
							toolTip:					qsTr("Edit the labels here or choose which values should be filtered out.")
							mouseArea.enabled:			false
							mouseArea.visible:			false
							//flickableInteractive:		false
							doubleClickWorkaround:		false

							Binding { target: columnModel; property: "rowWidth"; value: Math.max(levelsTableView.flickableWidth - 1, levelsTableView.filterColWidth + levelsTableView.valueColWidth + levelsTableView.labelColMinWidth + 2) }

							property real filterColWidth:	60  * jaspTheme.uiScale
							property real valueColWidth:	(columnModel.valueMaxWidth + 10) * jaspTheme.uiScale
							property real labelColMinWidth:	(columnModel.labelMaxWidth + 10) * jaspTheme.uiScale

							columnHeaderDelegate:	Item
							{
									z: -4
									Rectangle
									{
										color:						jaspTheme.uiBackground
										anchors.left:				parent.left
										anchors.rightMargin:		1
										height:						parent.height
										width:						levelsTableView.contentWidth
									}

									Row
									{
										anchors.left:				parent.left
										height:						parent.height
										Text
										{
											text:					qsTr("Filter")
											font:					jaspTheme.font
											color:					jaspTheme.textEnabled
											width:					levelsTableView.filterColWidth;
											anchors.verticalCenter:	parent.verticalCenter
											horizontalAlignment:	Text.AlignHCenter
										}
										Rectangle
										{
											width:					1
											height:					parent.height
											color:					jaspTheme.uiBorder
										}
										Text
										{
											text:					qsTr("Value")
											font:					jaspTheme.font
											color:					jaspTheme.textEnabled
											width:					levelsTableView.valueColWidth;
											leftPadding:			3 * jaspTheme.uiScale
											anchors.verticalCenter:	parent.verticalCenter
										}
										Rectangle
										{
											width:					1
											height:					parent.height
											color:					jaspTheme.uiBorder
										}
										Text
										{
											text:					qsTr("Label")
											font:					jaspTheme.font
											color:					jaspTheme.textEnabled
											leftPadding:			3 * jaspTheme.uiScale
											anchors.verticalCenter:	parent.verticalCenter
										}
									}
							}

							rowNumberDelegate:	Item { width: 0; height: 0; }

							itemDelegate: Item
							{
								id: backroundItem
								z:	-4

								Rectangle
								{
									id:				selectionRectangle
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

									MouseArea
									{
										anchors.fill:		selectionRectangle
										acceptedButtons:	Qt.LeftButton
										cursorShape:		Qt.PointingHandCursor
										z:					0
										hoverEnabled: 		true

										onClicked:			(mouse)=>
										{
											columnModel.setSelected(rowIndex, mouse.modifiers);
											selectionRectangle.forceActiveFocus(); //To take focus out of some TextInput
										}
										onDoubleClicked:	(mouse)=>
										{
											labelInput.forceActiveFocus()
										}
									}

									Row
									{
										QTC.Button
										{
											id:						filterCheckButton
											checkable:				true
											checked:				itemFiltered
											height:					backroundItem.height
											width:					levelsTableView.filterColWidth;
											anchors.top:			parent.top
											anchors.topMargin:		levelsTableView.itemVerticalPadding
											z:						-1

											onClicked:				if (!columnModel.setChecked(rowIndex, checked)) checked = true; // Case when all labels are unchecked.

											background: Item
											{
												Image
												{
													source:					filterCheckButton.checked ? jaspTheme.iconPath + "check-mark.png" : jaspTheme.iconPath + "cross.png"
													sourceSize.width:		Math.max(40, width)
													sourceSize.height:		Math.max(40, height)
													width:					height
													anchors
													{
														top:				parent.top
														bottom:				parent.bottom
														horizontalCenter:	parent.horizontalCenter
													}
												}
											}
										}

										Rectangle
										{
											width:					1
											height:					selectionRectangle.height
											color:					jaspTheme.uiBorder
										}

										Text
										{
											color:					jaspTheme.grayDarker
											text:					itemValue
											elide:					Text.ElideMiddle
											font:					jaspTheme.font
											leftPadding:			3 * jaspTheme.uiScale
											width:					levelsTableView.valueColWidth;
											anchors.top:			parent.top
											anchors.topMargin:		levelsTableView.itemVerticalPadding
											verticalAlignment:		Text.AlignVCenter

										}

										Rectangle
										{
											width:					1
											height:					selectionRectangle.height
											color:					jaspTheme.uiBorder
										}

										TextInput
										{
											id:					labelInput
											color:				jaspTheme.textEnabled

											text:				itemText
											font:				jaspTheme.font
											clip:				true
											selectByMouse:		true
											autoScroll:			true
											z:					1

											leftPadding:		3 * jaspTheme.uiScale
											width:				Math.min(Math.max(contentWidth, 20), parent.width + 20) + 3 * jaspTheme.uiScale//Minimal contentWidth to allow editing after label set to ""
											anchors.top:		parent.top
											anchors.topMargin:	levelsTableView.itemVerticalPadding

											verticalAlignment:	Text.AlignVCenter

											property int chosenColumnWas: -1

											onEditingFinished:
											{
												if(chosenColumnWas === columnModel.chosenColumn && rowIndex >= 0)
													columnModel.setLabel(rowIndex, text)
											}

											onActiveFocusChanged:
											{
												if (activeFocus)
												{
													chosenColumnWas = columnModel.chosenColumn
													columnModel.removeAllSelected()
												}
											}

											MouseArea
											{
												anchors.fill:		parent
												acceptedButtons:	Qt.NoButton
												cursorShape:		Qt.IBeamCursor
												hoverEnabled:		true
												z:					3
											}
										}
									}
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
						property real	minimumHeight:		(buttonHeight + 2 * spacing) * shownButtons - spacing
						property real	buttonHeight:		32 * preferencesModel.uiScale

						RoundedButton
						{
							//text: "UP"
							iconSource:		jaspTheme.iconPath + "arrow-up.png"

							onClicked:		columnModel.moveSelectionUp()
							toolTip:		qsTr("Move selected labels up")

							height:			buttonColumnVariablesWindow.buttonHeight
							implicitHeight: buttonColumnVariablesWindow.buttonHeight
							width:			height
						}

						RoundedButton
						{
							//text: "DOWN"
							iconSource:		jaspTheme.iconPath + "arrow-down.png"

							onClicked:		columnModel.moveSelectionDown()
							toolTip:		qsTr("Move selected labels down")

							height:			buttonColumnVariablesWindow.buttonHeight
							implicitHeight: buttonColumnVariablesWindow.buttonHeight
							width:			height
						}

						RoundedButton
						{
							//text: "REVERSE"
							iconSource:		jaspTheme.iconPath + "arrow-reverse.png"
							onClicked:		columnModel.reverse()

							toolTip:		qsTr("Reverse order of all labels")

							height:			buttonColumnVariablesWindow.buttonHeight
							implicitHeight: buttonColumnVariablesWindow.buttonHeight
							width:			height
						}

						RoundedButton
						{
							id:				eraseFiltersOnThisColumn
							iconSource:		jaspTheme.iconPath + "eraser.png"
							onClicked:		columnModel.resetFilterAllows()
							visible:		columnModel.filteredOut > 0

							toolTip:		qsTr("Reset all filter checkmarks for this column")

							height:			buttonColumnVariablesWindow.buttonHeight
							implicitHeight: buttonColumnVariablesWindow.buttonHeight
							width:			height
						}

						RoundedButton
						{
							id:				eraseFiltersOnAllColumns
							iconSource:		jaspTheme.iconPath + "eraser_all.png"
							onClicked:		dataSetModel.resetAllFilters()
							visible:		dataSetModel.columnsFilteredCount > (columnModel.filteredOut > 0 ? 1 : 0)
							height:			buttonColumnVariablesWindow.buttonHeight
							implicitHeight: buttonColumnVariablesWindow.buttonHeight
							width:			height

							toolTip:		qsTr("Reset all filter checkmarks for all columns")
						}

						Item //Spacer
						{
							Layout.fillHeight:	true
						}
					}
				}

			}
		}
	}
}


