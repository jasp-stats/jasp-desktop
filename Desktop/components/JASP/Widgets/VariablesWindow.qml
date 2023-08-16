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
	visible:	columnModel.visible && columnModel.chosenColumn > -1

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
				if(columnModel.columnName !== columnNameVariablesWindow.value) columnModel.columnName = columnNameVariablesWindow.value
				if(columnModel.columnTitle !== columnTitleVariablesWindow.value) columnModel.columnTitle = columnTitleVariablesWindow.value
				if(columnModel.columnDescription !== columnDescriptionVariablesWindow.text) columnModel.columnDescription = columnDescriptionVariablesWindow.text
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

		Keys.onPressed: (event) =>
		{
			var controlPressed	= Boolean(event.modifiers & Qt.ControlModifier);
			var shiftPressed	= Boolean(event.modifiers & Qt.ShiftModifier  );

			if (event.key === Qt.Key_Z)
			{
				if(controlPressed)
				{
					if (shiftPressed)
						columnModel.redo();
					else
						columnModel.undo();
					event.accepted = true;
				}
			}
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
			height:				columnNameVariablesWindow.height + columnDescriptionVariablesWindow.height + 3 * jaspTheme.generalAnchorMargin

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
				value:				columnModel.columnName
				onValueChanged:		if(columnModel.columnName !== value) columnModel.columnName = value
				undoModel:			columnModel

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
				fieldWidth:			columnNameVariablesWindow.fieldWidth * 1.5
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

			visible: columnModel.showLabelEditor || columnModel.showComputedColumn

			anchors.top:			common.bottom
			anchors.left:			parent.left
			anchors.right:			parent.right
			anchors.bottom:			parent.bottom
			anchors.margins:		jaspTheme.generalAnchorMargin
			anchors.rightMargin:	jaspTheme.generalAnchorMargin

			color: jaspTheme.uiBackground


			QTC.TabBar
			{
				id: bar
				contentHeight:	tabBarHeight + tabButtonRadius
				width:			editorTitles.length * tabButtonWidth
				background: Rectangle { color: jaspTheme.uiBackground }

				property real	tabBarHeight:		28 * preferencesModel.uiScale
				property real	tabButtonRadius:	5 * preferencesModel.uiScale
				property real	tabButtonWidth:		120 * preferencesModel.uiScale

				property var	editorTitles:		[ qsTr("Label Editor"), qsTr("Computed Column") ]
				property var	editorActive:		[ columnModel.showLabelEditor, columnModel.showComputedColumn ]
				property int	numEditors:			editorActive.reduce(function getSum(total, num) { return total + Math.round(num)}, 0);

				Connections
				{
					target: columnModel

					function onChosenColumnChanged(chosenColumn)
					{
						//if it is a computed column open that screen by default
						if(!columnModel.showComputedColumn || columnModel.columnIsFiltered)
							bar.currentIndex = 0
						else
							bar.currentIndex = 1
					}
				}

				Repeater
				{
					model: bar.editorTitles

					QTC.TabButton
					{
						height:		bar.height
						width:		bar.editorActive[index] ? bar.tabButtonWidth : 0
						visible:	bar.editorActive[index]
						background: Rectangle
						{
							color:			checked ? jaspTheme.uiBackground : jaspTheme.grayLighter
							radius:			bar.tabButtonRadius
							border.width:	1
							border.color:	checked ? jaspTheme.uiBorder : jaspTheme.borderColor
						}

						contentItem: Text
						{
							// The bottom of buttons are hidden to remove their bottom line with the radius
							// So the text has to be moved higher from the horizontal middle line.
							topPadding:			-bar.tabButtonRadius * 3/4
							text:				modelData
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
				width	: parent.width
				height	: bar.tabButtonRadius + 1
				anchors
				{
					left:		parent.left
					right:		bar.right
					top:		parent.top
					topMargin:	bar.tabBarHeight
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
				// Redraw a line below the unchecked tab
				anchors
				{
					top:			roundingHider.top
					left:			parent.left
					leftMargin:		bar.currentIndex === 0 || bar.numEditors === 1 ? bar.tabButtonWidth - 1 : 0
					right:			bar.right
					rightMargin:	bar.currentIndex === 0 || bar.numEditors === 1 ? 0 : bar.tabButtonWidth  - 1
				}
				height:	1
				color:	jaspTheme.uiBorder
				z:		1
			}

			Rectangle
			{
				// Rectangle to draw the border under the tabbar
				anchors
				{
					fill:		parent
					topMargin:	bar.tabBarHeight
				}
				z:				0
				border.width:	1
				border.color:	jaspTheme.uiBorder
				color:			"transparent"
			}


			StackLayout
			{
				currentIndex: bar.currentIndex

				anchors.top: bar.bottom
				anchors.bottom: parent.bottom
				anchors.left:	parent.left
				anchors.right:	parent.right
				anchors.margins: jaspTheme.generalAnchorMargin * 0.5
				anchors.topMargin: jaspTheme.generalAnchorMargin * 0.25

				Item
				{
					id: labelWindow
					Rectangle
					{
						anchors.fill:	parent
						color:			jaspTheme.uiBackground

						Rectangle
						{
							id:					tableBackground
							color:				jaspTheme.controlBackgroundColor
							border.color:		jaspTheme.uiBorder
							border.width:		1
							visible:			columnModel.showLabelEditor

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

							property int	shownButtons:		(columnModel.showLabelEditor ? 4 : 1) + (eraseFiltersOnThisColumn.visible ? 1 : 0) + (eraseFiltersOnAllColumns.visible ? 1 : 0)
							property real	minimumHeight:		!columnModel.showLabelEditor ? buttonHeight : (buttonHeight + 2 * spacing) * shownButtons - spacing
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
								visible:		columnModel.showLabelEditor
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
								visible:		columnModel.showLabelEditor
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
								visible:		columnModel.showLabelEditor
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

				Item
				{
					id: computedColumn
					Rectangle
					{
						anchors.fill: parent

						ComputeColumnWindow
						{
							id: computedColumnWindow
							visible:	true
							showName:	false
                            showColumnRemove: false
							anchors
							{
								left:			parent.left
								right:			parent.right
								top:			parent.top
								bottom:			parent.bottom
							}

							Connections
							{
								target: columnModel

								function onChosenColumnChanged(chosenColumn)
								{
									computedColumnWindow.open(columnModel.columnName);
								}
							}
						}
					}
				}
			}
		}
	}
}


