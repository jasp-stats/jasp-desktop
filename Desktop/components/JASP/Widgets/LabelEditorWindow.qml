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
				bottom:			parent.bottom
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
				//flickableInteractive:		false
				doubleClickWorkaround:		false

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
						levelsTableView.lastRow = -1;
					}
				}

				property real	filterColWidth:	60  * jaspTheme.uiScale
				property real	remainingWidth:	width - filterColWidth
				property real	valueColWidth:	Math.min(columnModel.valueMaxWidth + 10, remainingWidth * 0.5) * jaspTheme.uiScale
				property real	labelColWidth:	Math.min(columnModel.labelMaxWidth + 10, remainingWidth * 0.5) * jaspTheme.uiScale
				property int	lastRow:		-1
				

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
								width:					levelsTableView.labelColWidth;
							}
						}
				}

				rowNumberDelegate:	Item { width: 0; height: 0; }

				itemDelegate: FocusScope
				{
					id:						backgroundItem
					
					onActiveFocusChanged:	if(activeFocus)	levelsTableView.lastRow = rowIndex

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
						
						acceptedButtons:	Qt.LeftButton
						cursorShape:		Qt.DragHandCursor	
						z:					0

						onClicked:			(mouse)=>
						{
							columnModel.setSelected(rowIndex, mouse.modifiers);
							parent.forceActiveFocus(); //To take focus out of some TextInput
						}
						
						onDoubleClicked:	(mouse)=>
						{
							labelInput.forceActiveFocus()
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
							
								MouseArea
								{
									acceptedButtons:	Qt.LeftButton
									cursorShape:		Qt.IBeamCursor
									z:					3
									onClicked:			valueInput.forceActiveFocus()
									anchors
									{
										fill:			parent
										topMargin:		levelsTableView.itemVerticalPadding
										bottomMargin:	levelsTableView.itemVerticalPadding
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
										anchors.fill:		parent
										
		
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
												columnModel.removeAllSelected()
												columnModel.setSelected(rowIndex,true);
											}
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
								
								MouseArea
								{
									acceptedButtons:	Qt.LeftButton
									cursorShape:		Qt.IBeamCursor
									z:					3
									onClicked:			labelInput.forceActiveFocus()
									
									anchors
									{
										fill:			parent
										topMargin:		levelsTableView.itemVerticalPadding
										bottomMargin:	levelsTableView.itemVerticalPadding
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
										
										anchors.fill:		parent
										
		
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
									}
								}
							}
						}
					}
				}
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
				
				RoundedButton
				{
					iconSource:		jaspTheme.iconPath +  "menu-column-order-by-values.svg"
					onClicked:		{ forceActiveFocus(); columnModel.toggleAutoSortByValues(); }
	
					toolTip:		qsTr("Automatically order labels by their numeric value")
	
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
					visible:		!columnModel.autoSort || columnModel.firstNonNumericRow > 1 //if there are at least 2 numerics we have something to reverse
				}
				
				RoundedButton
				{
					iconSource:		jaspTheme.iconPath + "arrow-reverse.png"
					onClicked:		{ forceActiveFocus(); columnModel.reverse(); }
	
					toolTip:		columnModel.autoSort ? qsTr("Reverse order of the labels with non-numeric values") : qsTr("Reverse order of all labels")
	
					height:			buttonColumnVariablesWindow.buttonHeight
					implicitHeight: buttonColumnVariablesWindow.buttonHeight
					width:			height
					visible:		!columnModel.autoSort || columnModel.rowsTotal - columnModel.firstNonNumericRow > 1 //If there are at least 2 non numerics there is something to reverse
					
				}
	
				RoundedButton
				{
					iconSource:		jaspTheme.iconPath + "arrow-up.png"
	
					onClicked:		{ forceActiveFocus(); columnModel.moveSelectionUp(); }
					toolTip:		columnModel.autoSort ? qsTr("Move selected non-numeric labels up") : qsTr("Move selected labels up") 
	
					height:			buttonColumnVariablesWindow.buttonHeight
					implicitHeight: buttonColumnVariablesWindow.buttonHeight
					width:			height
					enabled:		levelsTableView.lastRow == -1 ? false : columnModel.firstNonNumericRow < levelsTableView.lastRow
					visible:		!columnModel.autoSort || columnModel.rowsTotal - columnModel.firstNonNumericRow > 1 //If there are at least 2 non numerics there is something to move up
				}
	
				RoundedButton
				{
					iconSource:		jaspTheme.iconPath + "arrow-down.png"
	
					onClicked:		{ forceActiveFocus(); columnModel.moveSelectionDown(); }
					toolTip:		columnModel.autoSort ? qsTr("Move selected non-numeric labels down") : qsTr("Move selected labels down")
	
					height:			buttonColumnVariablesWindow.buttonHeight
					implicitHeight: buttonColumnVariablesWindow.buttonHeight
					width:			height
					enabled:		levelsTableView.lastRow == -1  ? false :  ((columnModel.firstNonNumericRow <= levelsTableView.lastRow) && ( levelsTableView.lastRow < columnModel.rowsTotal - 1 ))
					visible:		!columnModel.autoSort || columnModel.rowsTotal - columnModel.firstNonNumericRow > 1 //If there are at least 2 non numerics there is something to move down
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
