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
				left:			buttonColumnFlickable.right
				right:			parent.right
				bottom:			newLabelContainer.top
				leftMargin:		jaspTheme.generalAnchorMargin
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

				Binding 
				{ 
					target:		columnModel
					property:	"rowWidth"
					value:		levelsTableView.width; //Math.max(levelsTableView.flickableWidth - 1, levelsTableView.filterColWidth + levelsTableView.valueColWidth + levelsTableView.labelColMinWidth + 2) 
				}
				
				Connections
				{
					target:		columnModel
					function	onChosenColumnChanged()
					{
						levelsTableView.selectedRow = -1;
					}
				}

				property real	filterColWidth:		60  * jaspTheme.uiScale
				property real	remainingWidth:		width - (2* filterColWidth)
				property real	valueColWidth:		Math.min(columnModel.valueMaxWidth + 10, remainingWidth * 0.5) * jaspTheme.uiScale
				property real	labelColWidth:		Math.min(columnModel.labelMaxWidth + 10, remainingWidth * 0.5) * jaspTheme.uiScale
				property int	selectedRow:		-1
				
				property bool	isBasicComputed:	columnModel.computedType == "rCode" || columnModel.computedType == "constructorCode"
				property bool	valueEditable:		!isBasicComputed || columnModel.currentColumnType != "scale"
				property bool	labelEditable:		!isBasicComputed || columnModel.currentColumnType == "scale"
				property bool	filterEditable:		true // columnModel.computedType == "notComputed"

				columnHeaderDelegate:	Item
				{
						z: -2
						Rectangle
						{
							color:						jaspTheme.uiBackground
							height:						parent.height
							width:						levelsTableView.width
						}

						Row
						{
							height:						parent.height
							Text
							{
								text:					qsTr("Filter")
								font:					jaspTheme.font
								color:					levelsTableView.filterEditable ? jaspTheme.textEnabled : jaspTheme.textDisabled
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
								color:					levelsTableView.valueEditable ? jaspTheme.textEnabled : jaspTheme.textDisabled
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
								color:					levelsTableView.labelEditable ? jaspTheme.textEnabled : jaspTheme.textDisabled
								leftPadding:			3 * jaspTheme.uiScale
								anchors.verticalCenter:	parent.verticalCenter
								width:					levelsTableView.remainingWidth - (levelsTableView.valueColWidth + 2 + (2 * levelsTableView.itemHorizontalPadding))
							}							
							Rectangle
							{
								width:					1
								height:					parent.height
								color:					jaspTheme.uiBorder
							}
							Text
							{
								text:					qsTr("Remove")
								font:					jaspTheme.font
								color:					jaspTheme.textEnabled
								anchors.verticalCenter:	parent.verticalCenter
								horizontalAlignment:	Text.AlignHCenter
								width:					levelsTableView.filterColWidth;
							}
						}
				}

				rowNumberDelegate:	Item { width: 0; height: 0; }

				itemDelegate: FocusScope
				{
					id:						backgroundItem
					
					onActiveFocusChanged:	if(activeFocus)	levelsTableView.selectedRow = rowIndex

					MouseArea
					{
						width:				levelsTableView.width
						anchors
						{
							top:			parent.top
							left:			parent.left
							bottom:			parent.bottom
							topMargin:		-levelsTableView.itemVerticalPadding
							leftMargin:		-levelsTableView.itemHorizontalPadding
							bottomMargin:	-levelsTableView.itemVerticalPadding
						}
						
						acceptedButtons:			Qt.LeftButton
						cursorShape:				Qt.DragHandCursor
						z:							0
						propagateComposedEvents:	true

						onClicked:			(mouse)=>
						{
							if(activeFocus)
								return;

							parent.forceActiveFocus(); //To take focus out of some TextInput
							columnModel.setSelected(rowIndex, mouse.modifiers);
							mouse.accepted = true;
						}
						
						onDoubleClicked:	(mouse)=>
						{
							labelInput.forceActiveFocus()
							mouse.accepted = true;
						}
					}

					Rectangle
					{
						id:					selectionRectangle
						color:				itemSelected ? jaspTheme.itemHighlight : "transparent"
						width:				levelsTableView.width
						anchors
						{
							top:			parent.top
							left:			parent.left
							bottom:			parent.bottom
							topMargin:		0.5-levelsTableView.itemVerticalPadding
							leftMargin:		-levelsTableView.itemHorizontalPadding
							bottomMargin:	0.5-levelsTableView.itemVerticalPadding
						}
						z:					-10
					}	
						
					Item
					{
						z:					100
						width:				levelsTableView.width
						anchors
						{
							top:			parent.top
							left:			parent.left
							bottom:			parent.bottom
							topMargin:		-levelsTableView.itemVerticalPadding
							leftMargin:		-levelsTableView.itemHorizontalPadding
							bottomMargin:	-levelsTableView.itemVerticalPadding
						}

						
					
					
					
						Row
						{
							id:					itemRow
							height:				parent.height
							z:					1
							
							MouseArea
							{
								id:						filterCheckButton
								width:					levelsTableView.filterColWidth;
								height:					parent.height
								z:						-1
								cursorShape:			Qt.PointingHandCursor
								enabled:				levelsTableView.filterEditable
								
	
								onClicked:				
								{
									columnModel.setChecked(rowIndex, !itemFiltered); // Case when all labels are unchecked.
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
										margins:			levelsTableView.itemVerticalPadding
										horizontalCenter:	filterCheckButton.horizontalCenter
									}
								}
							}
	
							Item
							{
								width:					1
								height:					parent.height
								
								Rectangle
								{
									x:						0.5
									width:					1
									height:					parent.height
									color:					jaspTheme.uiBorder
								}
							}
							
							Item
							{
								width:				levelsTableView.valueColWidth;
								height:				parent.height
								clip:				true
								enabled:			levelsTableView.valueEditable
							
								MouseArea
								{
									acceptedButtons:			Qt.LeftButton
									cursorShape:				Qt.IBeamCursor
									z:							3
									onClicked:					(mouse)=>
																{
																	if(valueInput.activeFocus)
																	{
																		mouse.accepted = false;
																		return
																	}

																	if(mouse.modifiers === Qt.NoModifier)
																		valueInput.forceActiveFocus();
																	else
																		levelsTableView.forceActiveFocus();

																	columnModel.setSelected(rowIndex, mouse.modifiers)
																	mouse.accepted = true; //dont let text input enable itself
																}
									onPressed:					(mouse) => { if(valueInput.activeFocus) mouse.accepted = false; }
									anchors.fill:				parent
									enabled:					!valueInput.activeFocus

								}

								TextInput
								{
									id:					valueInput
									color:				jaspTheme.textEnabled

									text:				itemValue
									font:				jaspTheme.font
									selectByMouse:		true
									autoScroll:			true
									z:					1

									leftPadding:		3 * jaspTheme.uiScale

									anchors
									{
										fill:			parent
										topMargin:		levelsTableView.itemVerticalPadding
										bottomMargin:	levelsTableView.itemVerticalPadding
									}


									verticalAlignment:	Text.AlignVCenter

									property int	chosenColumnWas: -1
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
										}
									}
								}
							}
							
							Item
							{
								width:					1
								height:					parent.height
								
								Rectangle
								{
									x:						0.5
									width:					1
									height:					parent.height
									color:					jaspTheme.uiBorder
								}
							}
	
							Item
							{
								
								width:				levelsTableView.remainingWidth - (levelsTableView.valueColWidth + 2 + (2 * levelsTableView.itemHorizontalPadding)) //+2 for line-rectangles!
								height:				parent.height
								clip:				true
								enabled:			levelsTableView.labelEditable
								
								MouseArea
								{
									acceptedButtons:			Qt.LeftButton
									cursorShape:				Qt.IBeamCursor
									z:							3
									onClicked:					(mouse)=>
																{
																	if(mouse.modifiers === Qt.NoModifier)
																		labelInput.forceActiveFocus();
																	else
																		levelsTableView.forceActiveFocus();

																	mouse.accepted = true; //dont let text input enable itself
																	columnModel.setSelected(rowIndex, mouse.modifiers)
																}
									
									anchors.fill:				parent
									enabled:					!labelInput.activeFocus
								}

								TextInput
								{
									id:					labelInput
									color:				jaspTheme.textEnabled

									text:				itemText
									font:				jaspTheme.font
									selectByMouse:		true
									autoScroll:			true
									z:					1
									//width:				contentWidth
									leftPadding:		3 * jaspTheme.uiScale

									anchors
									{
										fill:			parent
										topMargin:		levelsTableView.itemVerticalPadding
										bottomMargin:	levelsTableView.itemVerticalPadding
									}


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
										}
									}
								}
							}
						
							
							Item
							{
								width:					1
								height:					parent.height
								
								Rectangle
								{
									x:						0.5
									width:					1
									height:					parent.height
									color:					jaspTheme.uiBorder
								}
							}
							
							MouseArea
							{
								id:						deleteButton
								width:					levelsTableView.filterColWidth;
								height:					parent.height
								z:						-1
								cursorShape:			Qt.PointingHandCursor
								
	
								onClicked:				
								{
									columnModel.deleteLabel(rowIndex);
								}
								
								Text
								{
									text:					"⌫"
									font:					jaspTheme.font
									color:					jaspTheme.textEnabled
									anchors.verticalCenter:	parent.verticalCenter
									horizontalAlignment:	Text.AlignHCenter
									width:					levelsTableView.filterColWidth;
								}
	
								/*Image
								{
									source:					jaspTheme.iconPath + ("eraser.png")
									sourceSize.width:		Math.max(40, width)
									sourceSize.height:		Math.max(40, height)
									width:					height
									anchors
									{
										top:				deleteButton.top
										bottom:				deleteButton.bottom
										margins:			levelsTableView.itemVerticalPadding
										horizontalCenter:	deleteButton.horizontalCenter
									}
								}*/
							}
						}
					}
				}
			}

		}
		
		RowLayout
		{
			id:					newLabelContainer
			spacing:			0
			
			anchors
			{
				left:			tableBackground.left
				right:			parent.right
				bottom:			parent.bottom
			}
			
			Label
			{
				text:				qsTr("Add new level:")
				height:				parent.height
			}
			
			QTC.TextField
			{
				id:						newLevelValueInput
				
				font:					jaspTheme.font
				color:					jaspTheme.textEnabled
				selectedTextColor:		jaspTheme.white
				selectionColor:			jaspTheme.itemSelectedColor
				selectByMouse:			true
				Layout.minimumWidth:	levelsTableView.valueColWidth
				
				text:					""
				placeholderText:		qsTr("Value")
				height:					buttonColumnVariablesWindow.buttonHeight
				onEditingFinished:		if(activeFocus)	addLevelButton.clicked()
				
				background: Rectangle
				{
					
					color:				jaspTheme.controlBackgroundColor
					border.width:		1
					border.color:		jaspTheme.borderColor //If the border width is zero the color is inconsequential
					radius:				jaspTheme.borderRadius
					width:				parent.width   - 2 // (parent+self) border width
					height:				parent.height  - 2
					anchors.centerIn:	parent
				}
				
				
				
			}
			
			QTC.TextField
			{
				id:					newLevelLabelInput
				
				font:				jaspTheme.font
				color:				jaspTheme.textEnabled
				selectedTextColor:	jaspTheme.white
				selectionColor:		jaspTheme.itemSelectedColor
				selectByMouse:		true
				
				text:				""
				placeholderText:	qsTr("Label")
				height:				buttonColumnVariablesWindow.buttonHeight
				Layout.fillWidth:	true
				onEditingFinished:	if(activeFocus)	addLevelButton.clicked()
				
				background: Rectangle
				{
				
					color:				jaspTheme.controlBackgroundColor
					border.width:		1
					border.color:		jaspTheme.borderColor //If the border width is zero the color is inconsequential
					radius:				jaspTheme.borderRadius
					width:				parent.width   - 2 // (parent+self) border width
					height:				parent.height  - 2
					anchors.centerIn:	parent
				}
			}
			
			RoundedButton
			{
				id:				addLevelButton
				iconSource:		jaspTheme.iconPath +  "addition-sign-small.svg"
				onClicked:		
				{ 
					if(newLevelValueInput.text == "" && newLevelLabelInput.text == "")
					{
						newLevelValueInput.forceActiveFocus();
						return
					}
					
					var newValue = newLevelValueInput.text
					var newLabel = newLevelLabelInput.text
					
					if(newLabel == "")
						newLabel = newValue
						
					forceActiveFocus(); 
					columnModel.addLabel(newValue, newLabel); 
					
					newLevelValueInput.text = ""
					newLevelLabelInput.text = ""
				}

				toolTip:		qsTr("Add a level that's missing from the data")

				height:			implicitHeight
				implicitHeight: buttonColumnVariablesWindow.buttonHeight - newLevelValueInput.padding
				width:			height
			}
		}

		Flickable
		{
			id:					buttonColumnFlickable
			width:				buttonColumnVariablesWindow.width
			contentWidth:		buttonColumnVariablesWindow.width
			contentHeight:		buttonColumnVariablesWindow.height
			
			anchors
			{
				top:			parent.top
				left:			parent.left
				bottom:			parent.bottom
				leftMargin:		jaspTheme.generalAnchorMargin / 2
			}
			
			
			ColumnLayout
			{
				id:					buttonColumnVariablesWindow

				spacing:			Math.max(1, 2 * preferencesModel.uiScale)
	
				property int	shownButtons:		4 + (eraseFiltersOnThisColumn.visible ? 1 : 0) + (eraseFiltersOnAllColumns.visible ? 1 : 0)
				property real	buttonHeight:		32 * preferencesModel.uiScale

				//All the buttons do forceActiveFocus in an attempt to have any open textInputs edit's be resolved before doing any button related stuff
				RoundedButton
				{
					id:			dropLevelsButton
					iconSource: columnModel.dropLevels == "keep" ? jaspTheme.iconPath + "/eyeOpen.png" : jaspTheme.iconPath + "/eyeClosed.png"
					onClicked:	{ forceActiveFocus(); columnModel.dropLevels = (columnModel.dropLevels == "keep" ? "drop" : "keep") }
					width:		height
					toolTip:	(columnModel.dropLevels == "keep" ? qsTr("Keeping levels, so all levels are passed to the analysis, both filtered out ones and those added later manually.") : qsTr("Dropping levels, so only those levels in the (possibly filtered) data get sent to the analysis."))
					height:			buttonColumnVariablesWindow.buttonHeight
					implicitHeight: buttonColumnVariablesWindow.buttonHeight
				}
				
				RoundedButton
				{
					iconSource:		jaspTheme.iconPath +  "menu-column-order-by-values.svg"
					onClicked:		{ forceActiveFocus(); columnModel.toggleAutoSortByValues(); }
	
					toolTip:		qsTr("Automatically order labels by their value")
	
					height:			buttonColumnVariablesWindow.buttonHeight
					implicitHeight: buttonColumnVariablesWindow.buttonHeight
					width:			height
					color:			columnModel.autoSort ? jaspTheme.jaspBlue : jaspTheme.buttonColor
				}
				
				RoundedButton
				{
					iconSource:		jaspTheme.iconPath + "menu-column-reverse-values.svg"
					onClicked:		{ forceActiveFocus(); columnModel.reverseValues(); }
	
					toolTip:		qsTr("Reverse order of all numerical values")
	
					height:			buttonColumnVariablesWindow.buttonHeight
					implicitHeight: buttonColumnVariablesWindow.buttonHeight
					width:			height
					visible:		columnModel.hasSeveralNumericValues //if there are at least 2 numerics we have something to reverse
				}
				
				RoundedButton
				{
					iconSource:		jaspTheme.iconPath + "arrow-reverse.png"
					onClicked:		{ forceActiveFocus(); columnModel.reverse(); }
	
					toolTip:		!columnModel.autoSort ? qsTr("Reverse order of all labels") : qsTr("Turn automatically ordering off to reverse order of all labels")
	
					height:			buttonColumnVariablesWindow.buttonHeight
					implicitHeight: buttonColumnVariablesWindow.buttonHeight
					width:			height
					enabled:		!columnModel.autoSort && columnModel.rowsTotal > 1
				}
	
				RoundedButton
				{
					iconSource:		jaspTheme.iconPath + "arrow-up.png"
	
					onClicked:		{ forceActiveFocus(); columnModel.moveSelectionUp(); levelsTableView.selectedRow--; }
					toolTip:		!columnModel.autoSort ? qsTr("Move selected labels up") : qsTr("Turn automatically ordering off to move labels up manually")
	
					height:			buttonColumnVariablesWindow.buttonHeight
					implicitHeight: buttonColumnVariablesWindow.buttonHeight
					width:			height
					enabled:		!columnModel.autoSort && levelsTableView.selectedRow > 0
				}
	
				RoundedButton
				{
					iconSource:		jaspTheme.iconPath + "arrow-down.png"
	
					onClicked:		{ forceActiveFocus(); columnModel.moveSelectionDown(); levelsTableView.selectedRow++; }
					toolTip:		!columnModel.autoSort ? qsTr("Move selected labels down") : qsTr("Turn automatically ordering off to move labels down manually")
	
					height:			buttonColumnVariablesWindow.buttonHeight
					implicitHeight: buttonColumnVariablesWindow.buttonHeight
					width:			height
					enabled:		!columnModel.autoSort && levelsTableView.selectedRow >= 0 && levelsTableView.selectedRow < columnModel.rowCount() - 1
				}
	
				RoundedButton
				{
					id:				eraseFiltersOnThisColumn
					iconSource:		jaspTheme.iconPath + "eraser.png"
					onClicked:		{ forceActiveFocus(); columnModel.resetFilterAllows(); }
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
					onClicked:		{ forceActiveFocus(); dataSetModel.resetAllFilters(); }
					visible:		dataSetModel.columnsFilteredCount > (columnModel.filteredOut > 0 ? 1 : 0)
					height:			buttonColumnVariablesWindow.buttonHeight
					implicitHeight: buttonColumnVariablesWindow.buttonHeight
					width:			height
	
					toolTip:		qsTr("Reset all filter checkmarks for all columns")
				}
			}
		}
	}
}
