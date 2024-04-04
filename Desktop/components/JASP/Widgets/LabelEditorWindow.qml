import QtQuick
import QtQuick.Layouts
import JASP.Controls
import QtQuick.Controls as QTC

FocusScope
{
	Rectangle
	{
		color:			jaspTheme.uiBackground
		anchors.fill:	parent

		Rectangle
		{
			id:					tableBackground
			color:				jaspTheme.controlBackgroundColor
			border.color:		jaspTheme.uiBorder
			border.width:		1

			anchors
			{
				top:			parent.top
				left:			buttonColumnVariablesWindow.right
				right:			parent.right
				bottom:			parent.bottom
				leftMargin:	jaspTheme.generalAnchorMargin
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
							cursorShape:		Qt.DragHandCursor	
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
							MouseArea
							{
								id:						filterCheckButton
								height:					backroundItem.height
								width:					levelsTableView.filterColWidth;
								anchors.top:			parent.top
								anchors.topMargin:		levelsTableView.itemVerticalPadding
								z:						-1
								cursorShape:			Qt.PointingHandCursor
								

								onClicked:				
								{
									columnModel.setChecked(rowIndex, !itemFiltered); // Case when all labels are unchecked.
									columnModel.setSelected(rowIndex,true);
								}

								Image
								{
									source:					jaspTheme.iconPath + (itemFiltered ? "check-mark.png" : "cross.png")
									sourceSize.width:		Math.max(40, width)
									sourceSize.height:		Math.max(40, height)
									width:					height
									anchors
									{
										top:				filterCheckButton.top
										bottom:				filterCheckButton.bottom										
										horizontalCenter:	filterCheckButton.horizontalCenter
									}
								}
							}

							Rectangle
							{
								width:					1
								height:					selectionRectangle.height
								color:					jaspTheme.uiBorder
							}
							
							TextInput
							{
								id:					valueInput
								color:				jaspTheme.textEnabled

								text:				itemValue
								font:				jaspTheme.font
								clip:				true
								selectByMouse:		true
								autoScroll:			true
								z:					1

								leftPadding:		3 * jaspTheme.uiScale
								width:				levelsTableView.valueColWidth;
								//width:				Math.min(Math.max(contentWidth, 20), parent.width + 20) + 3 * jaspTheme.uiScale//Minimal contentWidth to allow editing after label set to ""
								anchors.top:		parent.top
								anchors.topMargin:	levelsTableView.itemVerticalPadding

								verticalAlignment:	Text.AlignVCenter

								property int chosenColumnWas: -1
								property string lastActiveText: ""
								property int	lastActiveRow:	-1
								
								onEditingFinished:
								{
									messages.log("Label value editing finished, '%1' was entered for row %2 and %3".arg(text).arg(rowIndex).arg((activeFocus ? "activeFocus!" : focus ? "focus." : "no focus.")))
									
									//If we press enter in the thing we get this slot fired twice, once with activeFocus and once without focus
									//To ignore this here some ugly faintly persistent memory
									var shouldISet = activeFocus || lastActiveRow !== rowIndex || lastActiveText !== text
									
									if(shouldISet && chosenColumnWas === columnModel.chosenColumn && rowIndex >= 0)
									{
										columnModel.setValue(rowIndex, text)
										
										lastActiveRow  = activeFocus ? rowIndex		:	-1
										lastActiveText = activeFocus ? text			:	""
									}
								}
								onActiveFocusChanged:
								{
									if (activeFocus)
									{
										chosenColumnWas = columnModel.chosenColumn
										columnModel.removeAllSelected()
										columnModel.setSelected(rowIndex,true);
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
								
								property string lastActiveText: ""
								property int	lastActiveRow:	-1
								
								onEditingFinished:
								{
									messages.log("Label label editing finished, '%1' was entered for row %2 and %3".arg(text).arg(rowIndex).arg((activeFocus ? "activeFocus!" : focus ? "focus." : "no focus.")))
									
									//If we press enter in the thing we get this slot fired twice, once with activeFocus and once without focus
									//To ignore this here some ugly faintly persistent memory
									var shouldISet = activeFocus || lastActiveRow !== rowIndex || lastActiveText !== text
									
									if(shouldISet && chosenColumnWas === columnModel.chosenColumn && rowIndex >= 0)
									{
										columnModel.setLabel(rowIndex, text)
										
										lastActiveRow  = activeFocus ? rowIndex		:	-1
										lastActiveText = activeFocus ? text			:	""
									}
								}

								onActiveFocusChanged:
								{
									if (activeFocus)
									{
										chosenColumnWas = columnModel.chosenColumn
										columnModel.removeAllSelected()
										columnModel.setSelected(rowIndex,true);
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
			anchors.left:		parent.left
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
