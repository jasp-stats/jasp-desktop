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
