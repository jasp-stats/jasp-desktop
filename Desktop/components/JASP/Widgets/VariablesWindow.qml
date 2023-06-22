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
	visible:	labelModel.visible && labelModel.chosenColumn > -1

	property real calculatedBaseHeight:			columnNameVariablesWindow.height + (jaspTheme.generalAnchorMargin * 2)
	property real calculatedMinimumHeight:		columnDescriptionVariablesWindow.height		+ calculatedBaseHeight + buttonColumnVariablesWindow.minimumHeight
	property real calculatedPreferredHeight:	columnDescriptionVariablesWindow.height		+ calculatedBaseHeight + buttonColumnVariablesWindow.minimumHeight
	property real calculatedMaximumHeight:		columnDescriptionVariablesWindow.maxHeight	+ calculatedBaseHeight + Math.max(buttonColumnVariablesWindow.minimumHeight, labelModel.showLabelsEditing ? parent.height * 0.6666667 : 0)


	Connections
	{
		target: labelModel
		
		function onChosenColumnChanged(chosenColumn)
		{
			if(labelModel.chosenColumn > -1 && labelModel.chosenColumn < dataSetModel.columnCount())
				//to prevent the editText in the labelcolumn to get stuck and overwrite the next columns data... We have to remove activeFocus from it
				levelsTableViewRectangle.focus = true //So we just put it somewhere
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
			id:					levelsTableViewRectangle
			anchors.fill:		parent
			anchors.margins:	jaspTheme.generalAnchorMargin

			property int labelMaxWidth: Math.max(nameLabel.width, descriptionLabel.width)

			Label
			{
				id: nameLabel
				text: qsTr("Name: ")
				anchors
				{
					top:			parent.top
					left:			parent.left
				}
			}

			Label
			{
				id: descriptionLabel
				text: qsTr("Description: ")
				anchors
				{
					top:			nameLabel.bottom
					topMargin:		jaspTheme.generalAnchorMargin
					left:			parent.left
				}
			}

			TextField
			{
				id:					columnNameVariablesWindow
				value:				labelModel.columnName
				onValueChanged:		if(labelModel.columnName !== value) labelModel.columnName = value

				anchors
				{
					left:			parent.left
					leftMargin:		levelsTableViewRectangle.labelMaxWidth + jaspTheme.generalAnchorMargin
					top:			parent.top
				}
			}


			TextField
			{
				id:					columnTitleVariablesWindow
				label:				qsTr("Title: ");
				value:				labelModel.columnTitle
				onValueChanged:		if(labelModel.columnTitle !== value) labelModel.columnTitle = value

				anchors
				{
					left:			columnNameVariablesWindow.right
					leftMargin:		jaspTheme.generalAnchorMargin
					top:			parent.top
				}
			}

			TextArea
			{
				id:					columnDescriptionVariablesWindow
				anchors
				{
					top:			descriptionLabel.top
					left:			parent.left
					leftMargin:		levelsTableViewRectangle.labelMaxWidth + jaspTheme.generalAnchorMargin
				}
				height:				Math.max(columnNameVariablesWindow.height, Math.min(maxHeight, control.contentHeight + 5 * jaspTheme.uiScale))
				width:				parent.width - x
				control.padding:	3 * jaspTheme.uiScale

				text:				labelModel.columnDescription
				onTextChanged:		if(labelModel.columnDescription !== text) labelModel.columnDescription = text
				applyScriptInfo:	""
				placeholderText:	"..."

				property int maxHeight:	100 * jaspTheme.uiScale
			}

			Rectangle
			{
				id:					tableBackground
				color:				jaspTheme.controlBackgroundColor
				border.color:		jaspTheme.uiBorder
				border.width:		1
				visible:			labelModel.showLabelsEditing

				anchors
				{
					top:			columnDescriptionVariablesWindow.bottom
					left:			parent.left
					right:			buttonColumnVariablesWindow.left
					bottom:			parent.bottom
					topMargin:		jaspTheme.generalAnchorMargin
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

					model:						labelModel
					cacheItems:					false
					expandDataSet:				false
					toolTip:					qsTr("Edit the labels here or choose which values should be filtered out.")
					mouseArea.enabled:			false
					mouseArea.visible:			false
					//flickableInteractive:		false
					doubleClickWorkaround:		false

					Binding { target: labelModel; property: "rowWidth"; value: Math.max(levelsTableView.flickableWidth - 1, levelsTableView.filterColWidth + levelsTableView.valueColWidth + levelsTableView.labelColMinWidth + 2) }

					property real filterColWidth:	60  * jaspTheme.uiScale
					property real valueColWidth:	(labelModel.valueMaxWidth + 10) * jaspTheme.uiScale
					property real labelColMinWidth:	(labelModel.labelMaxWidth + 10) * jaspTheme.uiScale

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
									labelModel.setSelected(rowIndex, mouse.modifiers);
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

									onClicked:				if (!labelModel.setChecked(rowIndex, checked)) checked = true; // Case when all labels are unchecked.

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
										if(chosenColumnWas === labelModel.chosenColumn && rowIndex >= 0)
											labelModel.setLabel(rowIndex, text)
									}

									onActiveFocusChanged:
									{
										if (activeFocus)
										{
											chosenColumnWas = labelModel.chosenColumn
											labelModel.removeAllSelected()
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

				anchors.top:		tableBackground.top
				anchors.right:		parent.right
				anchors.bottom:		parent.bottom
				spacing:			Math.max(1, 2 * preferencesModel.uiScale)

				property int	shownButtons:		(labelModel.showLabelsEditing ? 4 : 1) + (eraseFiltersOnThisColumn.visible ? 1 : 0) + (eraseFiltersOnAllColumns.visible ? 1 : 0)
				property real	minimumHeight:		(buttonHeight + 2 * spacing) * shownButtons - spacing
				property real	buttonHeight:		32 * preferencesModel.uiScale

				RoundedButton
				{
					//text: "UP"
					iconSource:		jaspTheme.iconPath + "arrow-up.png"

					onClicked:		labelModel.moveSelectionUp()
					toolTip:		qsTr("Move selected labels up")

					height:			buttonColumnVariablesWindow.buttonHeight
					implicitHeight: buttonColumnVariablesWindow.buttonHeight
					width:			height
					visible:		labelModel.showLabelsEditing
				}

				RoundedButton
				{
					//text: "DOWN"
					iconSource:		jaspTheme.iconPath + "arrow-down.png"

					onClicked:		labelModel.moveSelectionDown()
					toolTip:		qsTr("Move selected labels down")

					height:			buttonColumnVariablesWindow.buttonHeight
					implicitHeight: buttonColumnVariablesWindow.buttonHeight
					width:			height
					visible:		labelModel.showLabelsEditing
				}

				RoundedButton
				{
					//text: "REVERSE"
					iconSource:		jaspTheme.iconPath + "arrow-reverse.png"
					onClicked:		labelModel.reverse()

					toolTip:		qsTr("Reverse order of all labels")

					height:			buttonColumnVariablesWindow.buttonHeight
					implicitHeight: buttonColumnVariablesWindow.buttonHeight
					width:			height
					visible:		labelModel.showLabelsEditing
				}

				RoundedButton
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

				RoundedButton
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

				RoundedButton
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
