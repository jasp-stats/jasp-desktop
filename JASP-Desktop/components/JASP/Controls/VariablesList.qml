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


import QtQuick 2.11
import QtQuick.Controls 2.4 as QTCONTROLS
import QtQml.Models 2.2
import JASP.Theme 1.0
import QtQuick.Layouts 1.3

JASPControl
{
	id:					variablesList
	controlType:		"VariablesListView"
	background:			rectangle
	width:				parent.width
	implicitWidth:		width
	height:				singleVariable ? Theme.defaultSingleItemListHeight : Theme.defaultListHeight
	implicitHeight:		height
	useControlMouseArea:	false
	
	property var	model
	property string title
	property alias	label:				variablesList.title
	property alias	count:				listView.count
	property int	columns:			1
	property string itemType:			"variables"
	property alias	dropKeys:			dropArea.keys
	property string	dropMode:			"None"
	property bool	draggable:			true
	property var	source
	property alias	syncModels:			variablesList.source
	property bool	singleVariable:		false
	property string listViewType:		"AvailableVariables"
	property var	allowedColumns:		[]
	property bool	dropModeInsert:		dropMode === "Insert"
	property bool	dropModeReplace:	dropMode === "Replace"
	property alias	selectedItems:		listView.selectedItems
	property var	selectedItemsTypes:	[]
	property var	suggestedColumns:	[]
	property bool	showElementBorder:	false
	property bool	dragOnlyVariables:	false
	property bool	showVariableTypeIcon:	true
	property bool	setWidthInForm:		false
	property bool	setHeightInForm:	false
	property bool	mustContainLowerTerms: true
	property bool	addAvailableVariablesToAssigned: listViewType === "Interaction"
	
	property var	interactionControl
	property bool	addInteractionOptions:	false
	
	property var	extraControlColumns:		[]
	property string extraControlOptionName:		""
	property alias	extraControlTitles:	titles.model
	
	property int	indexInDroppedListViewOfDraggedItem:	-1
	
	readonly property int rectangleY: rectangle.y
	
	signal itemDoubleClicked(int index);
	signal itemsDropped(var indexes, var dropList, int dropItemIndex, string assignOption);
	signal hasSelectedItemsChanged();

	function setSelectedItems()
	{
		var items = listView.getExistingItems()
		variablesList.selectedItemsTypes = []
		for (var i = 0; i < items.length; i++)
		{
			var item = items[i]
			if (listView.selectedItems.includes(item.rank))
			{
				item.selected = true
				if (!variablesList.selectedItemsTypes.includes(item.columnType))
					variablesList.selectedItemsTypes.push(item.columnType)
			}
			else
				item.selected = false;
		}

		hasSelectedItemsChanged();
	}

	function moveSelectedItems(target)
	{
		if (listView.selectedItems.length === 0) return;
		
		var assignOption = target.interactionControl ? target.interactionControl.model.get(target.interactionControl.currentIndex).value : ""
		itemsDropped(selectedItems, target, -1, assignOption);
		listView.clearSelectedItems(true);
	}
	
	DropArea
	{
		id: dropArea
		anchors.fill: parent
		
		property bool canDrop: containsDrag && (variablesList.allowedColumns.length === 0 || variablesList.allowedColumns.indexOf(drag.source.columnType) >=0 )
		
		onPositionChanged:
		{
			if (variablesList.singleVariable || (!variablesList.dropModeInsert && !variablesList.dropModeReplace)) return;
			var itemIndex = Math.floor((drag.y - text.height) / listView.cellHeight);
			if (variablesList.columns > 1)
			{
				itemIndex = itemIndex * 2 + Math.floor(drag.x / listView.cellWidth);
			}
			
			if (itemIndex >= 0 && itemIndex < listView.contentItem.children.length)
			{
				var item = listView.contentItem.children[itemIndex].children[0];
				if (item && item.objectName === "itemRectangle") {
					listView.itemContainingDrag = item
					variablesList.indexInDroppedListViewOfDraggedItem = itemIndex
				}
				else
				{
					console.log("dropArea: could not find child!")
				}
			}
			else
			{
				listView.itemContainingDrag = null
				variablesList.indexInDroppedListViewOfDraggedItem = -1
			}
		}
		onExited:
		{
			listView.itemContainingDrag = null
			variablesList.indexInDroppedListViewOfDraggedItem = -1
		}
	}
	
	Text
	{
		id:				text
		anchors.top:	parent.top
		anchors.left:	parent.left
		text:			title
		height:			title ? Theme.variablesListTitle : 0
		font:			Theme.font
		color:			enabled ? Theme.textEnabled : Theme.textDisabled
		
	}
	
	Row
	{
		width:				parent.width
		anchors.top:		variablesList.top;
		spacing:			1
		layoutDirection:	Qt.RightToLeft
		Repeater
		{
			id: titles;
			Label { text: modelData }
		}
	}
	
	Rectangle
	{
		id:				rectangle
		anchors.top:	text.bottom
		anchors.left:	parent.left
		height:			variablesList.height - text.height
		width:			parent.width
		color:			debug ? Theme.debugBackgroundColor : Theme.controlBackgroundColor
		border.width:	1
		border.color:	dropArea.canDrop ? Theme.containsDragBorderColor : Theme.borderColor
		
		states: [
			State
			{
				when: dropArea.canDrop
				PropertyChanges
				{
					target:			rectangle
					border.width:	4  * preferencesModel.uiScale
					radius:			3 * preferencesModel.uiScale
				}
			}
		]

		Repeater
		{
			model: suggestedColumns


			Image
			{
				source: enabled ? iconInactiveFiles[suggestedColumns[index]] : iconDisabledFiles[suggestedColumns[index]]
				height: 16 * preferencesModel.uiScale
				width:	16 * preferencesModel.uiScale
				z:		2
				anchors
				{
					bottom:			rectangle.bottom;
					bottomMargin:	4  * preferencesModel.uiScale
					right:			rectangle.right;
					rightMargin:	(index * 20 + 4)  * preferencesModel.uiScale + (scrollBar.visible ? scrollBar.width : 0)
				}
			}
		}
		
		Component.onCompleted:
		{
			var mySuggestedColumns = []
			var myAllowedColumns = []

			if (typeof suggestedColumns === "string")
				mySuggestedColumns.push(suggestedColumns)
			else
				mySuggestedColumns = suggestedColumns.concat()
			if (typeof allowedColumns === "string")
				myAllowedColumns.push(allowedColumns)
			else
				myAllowedColumns = allowedColumns.concat()

			if (mySuggestedColumns.length === 0 && myAllowedColumns.length > 0)
				mySuggestedColumns = myAllowedColumns.concat()
			else if (myAllowedColumns.length === 0 && mySuggestedColumns.length > 0)
			{
				myAllowedColumns = mySuggestedColumns.concat()
				if (mySuggestedColumns.includes("scale"))
				{
					if (!myAllowedColumns.includes("nominal"))
						myAllowedColumns.push("nominal")
					if (!myAllowedColumns.includes("ordinal"))
						myAllowedColumns.push("ordinal")
				}
				if (mySuggestedColumns.includes("nominal"))
				{
					if (!myAllowedColumns.includes("nominalText"))
						myAllowedColumns.push("nominalText")
					if (!myAllowedColumns.includes("ordinal"))
						myAllowedColumns.push("ordinal")
				}
			}
			suggestedColumns = mySuggestedColumns.concat()
			allowedColumns = myAllowedColumns.concat()
			
			var length = variablesList.resources.length
			for (var i = length - 1; i >= 0; i--)
			{
				var column = variablesList.resources[i];
				if (column instanceof ExtraControlColumn)
					variablesList.extraControlColumns.push(column);
			}
		}

		JASPScrollBar
		{
			id:				scrollBar
			flickable:		listView
			manualAnchor:	true
			vertical:		true
			z:				1337

			anchors
			{
				top:		parent.top
				right:		parent.right
				bottom:		parent.bottom
				margins:	2
			}
		}
		
		GridView
		{
			id:						listView
			cellHeight:				20  * preferencesModel.uiScale
			cellWidth:				width / variablesList.columns
			clip:					true
			focus:					true
			anchors.fill:			parent
			anchors.margins:		4  * preferencesModel.uiScale
			anchors.rightMargin:	scrollBar.width + anchors.margins
			model:					variablesList.model
			delegate:				itemComponent
			boundsBehavior:			Flickable.StopAtBounds
			
			property int startShiftSelected: 0;
			property int endShiftSelected: -1;
			property var selectedItems: [];
			property bool mousePressed: false;
			property bool shiftPressed: false;
			property var itemContainingDrag
			property var draggingItems: []
			
			onCurrentItemChanged:
			{
				if (shiftPressed)
				{
					if (endShiftSelected >= 0)
						selectShiftItems(false);
					endShiftSelected = listView.currentIndex;
					selectShiftItems(true);
				}
				else if (!mousePressed)
				{
					var itemWrapper = listView.currentItem;
					if (itemWrapper)
					{
						var itemRectangle = itemWrapper.children[0];
						listView.clearSelectedItems(false);
						listView.addSelectedItem(itemRectangle.rank);
						listView.startShiftSelected = listView.currentIndex;
						listView.endShiftSelected = -1;
					}
				}
			}
			
			Keys.onPressed:
			{
				if (event.modifiers & Qt.ShiftModifier || event.key === Qt.Key_Shift)
					shiftPressed = true;
				else
					shiftPressed = false;
			}
			
			Keys.onReleased:
			{
				if (event.modifiers & Qt.ShiftModifier || event.key === Qt.Key_Shift)
					shiftPressed = false;
			}
			
			Keys.onSpacePressed:
			{
				moveSelectedItems()
			}
			Keys.onReturnPressed:
			{
				moveSelectedItems()
			}

			function getExistingItems()
			{
				var items = [];
				for (var i = 0; i < listView.contentItem.children.length; i++)
				{
					var item = listView.contentItem.children[i];
					if (item.children.length === 0)
						continue;
					item = item.children[0];
					if (item.objectName === "itemRectangle")
						items.push(item);
				}

				return items;
			}
			
			function addSelectedItem(itemRank)
			{
				if (selectedItems.includes(itemRank))
					return;
				
				selectedItems.push(itemRank);
				selectedItems.sort();
				variablesList.setSelectedItems()
			}
			
			function removeSelectedItem(itemRank)
			{
				var index = selectedItems.indexOf(itemRank)
				if (index >= 0)
				{
					selectedItems.splice(index, 1);
					variablesList.setSelectedItems()
				}
			}
			
			function clearSelectedItems(emitSignal)
			{
				selectedItems = [];
				if (emitSignal)
					variablesList.setSelectedItems()
			}
			
			function selectShiftItems(selected)
			{
				var startIndex = listView.startShiftSelected;
				var endIndex = listView.endShiftSelected;
				if (startIndex > endIndex)
				{
					var temp = startIndex;
					startIndex = endIndex;
					endIndex = temp;
				}

				if (selected)
				{
					for (var i = startIndex; i <= endIndex; i++)
					{
						if (!listView.selectedItems.includes(i))
							listView.selectedItems.push(i)
					}
					listView.selectedItems.sort();
				}
				else
				{
					for (var i = startIndex; i <= endIndex; i++)
					{
						var index = selectedItems.indexOf(i)
						if (index >= 0)
							selectedItems.splice(index, 1);
					}
				}
				variablesList.setSelectedItems()
			}
		}
	}
	
	Component
	{
		id: itemComponent

		FocusScope
		{
			id:			itemWrapper
			height:		listView.cellHeight
			width:		listView.cellWidth
			
			Rectangle
			{
				id:							itemRectangle
				objectName:					"itemRectangle"
				anchors.horizontalCenter:	parent.horizontalCenter
				anchors.verticalCenter:		parent.verticalCenter
				// the height & width of itemWrapper & itemRectangle must be set independently of each other:
				// when the rectangle is dragged, it gets another parent but it must keep the same size,
				height:			listView.cellHeight
				width:			listView.cellWidth
				focus:			true
				border.width:	containsDragItem && variablesList.dropModeReplace ? 2 : (variablesList.showElementBorder ? 1 : 0)
				border.color:	containsDragItem && variablesList.dropModeReplace ? Theme.containsDragBorderColor : Theme.grayLighter
				
				
				property bool clearOtherSelectedItemsWhenClicked: false
				property bool selected:				listView.selectedItems.includes(rank)
				property bool dragging:				false
				property int offsetX:				0
				property int offsetY:				0
				property int rank:					index
				property bool containsDragItem:		listView.itemContainingDrag === itemRectangle
				property bool isVirtual:			(typeof model.type !== "undefined") && model.type.includes("virtual")
				property bool isVariable:			(typeof model.type !== "undefined") && model.type.includes("variable")
				property bool isLayer:				(typeof model.type !== "undefined") && model.type.includes("layer")
				property bool draggable:			variablesList.draggable && (!variablesList.dragOnlyVariables || isVariable)
				property string columnType:			isVariable && (typeof model.columnType !== "undefined") ? model.columnType : ""
				property var extraColumnsModel:		model.extraColumns

				enabled: variablesList.listViewType != "AvailableVariables" || !columnType || variablesList.allowedColumns.length == 0 || (variablesList.allowedColumns.indexOf(columnType) >= 0)
				
				function setRelative(draggedRect)
				{
					x = Qt.binding(function (){ return draggedRect.x + offsetX; })
					y = Qt.binding(function (){ return draggedRect.y + offsetY; })
				}
				
				color:
				{
					if (!itemRectangle.draggable)											return Theme.controlBackgroundColor;
					else if (itemRectangle.selected)											return variablesList.activeFocus ? Theme.itemSelectedColor: Theme.itemSelectedNoFocusColor;
					else if (itemRectangle.containsDragItem && variablesList.dropModeReplace)	return Theme.itemSelectedColor;
					else if (mouseArea.containsMouse)											return Theme.itemHoverColor;
					else																		return Theme.controlBackgroundColor;
				}
				Drag.keys:		[variablesList.name]
				Drag.active:	mouseArea.drag.active
				Drag.hotSpot.x:	itemRectangle.width / 2
				Drag.hotSpot.y:	itemRectangle.height / 2
				
				// Use the ToolTip Attached property to avoid creating ToolTip object for each item
				QTCONTROLS.ToolTip.visible: mouseArea.containsMouse && model.name && !itemRectangle.containsDragItem
				QTCONTROLS.ToolTip.delay: 300
				QTCONTROLS.ToolTip.text: model.name
				
				Rectangle
				{
					height:		2
					width:		parent.width
					color:		Theme.red
					visible:	itemRectangle.containsDragItem && variablesList.dropModeInsert
				}
				
				Image
				{
					id:						icon
					height:					15 * preferencesModel.uiScale
					width:					15 * preferencesModel.uiScale
					anchors.verticalCenter:	parent.verticalCenter
					source:					(!(variablesList.showVariableTypeIcon && itemRectangle.isVariable) || !model.columnType) ? "" : enabled ? iconFiles[model.columnType] : iconDisabledFiles[model.columnType]
					visible:				variablesList.showVariableTypeIcon && itemRectangle.isVariable
				}
				Text
				{
					id:						colName
					x:						(variablesList.showVariableTypeIcon ? 20 : 4) * preferencesModel.uiScale
					text:					model.name
					width:					itemRectangle.width - x
					elide:					Text.ElideRight
					anchors.verticalCenter:	parent.verticalCenter
					horizontalAlignment:	itemRectangle.isLayer ? Text.AlignHCenter : undefined
					color:					!enabled ? Theme.textDisabled : itemRectangle.isVirtual ? Theme.grayLighter : (itemRectangle.color === Theme.itemSelectedColor ? Theme.white : Theme.black)
					font:					Theme.font
				}
				
				RowLayout
				{
					anchors.verticalCenter:	parent.verticalCenter
					anchors.right:			parent.right
					anchors.rightMargin:	10  * preferencesModel.uiScale
					spacing:				1
					z:						10
					
					layoutDirection: Qt.RightToLeft
					
					Repeater
					{
						model: itemRectangle.extraColumnsModel
						
						delegate: Loader
						{
							sourceComponent: model.type === "CheckBox" ? extraCheckBoxComponent :
											(	(model.type === "ComboBox" || model.type === "Dropdown") ? extraComboBoxComponent :
													(model.type === "IntegerField" ? extraIntegerFieldComponent :
														extraTextFieldComponent
													 )
											 )
							asynchronous:	false

							property double extraControlHeight:		itemRectangle.height
							property string extraControlColName:    colName.text
							property var    extraControlModel:      itemRectangle.extraColumnsModel
							property string extraControlName:       model.name
							property var    extraControlProperties: model.properties
						}
					}
				}
				
				states: [
					State
					{
						when: itemRectangle.dragging
						ParentChange
						{
							target: itemRectangle
							parent: form
						}
						AnchorChanges
						{
							target: itemRectangle
							anchors.horizontalCenter: undefined
							anchors.verticalCenter: undefined
						}
						PropertyChanges
						{
							target: itemRectangle
							opacity: 0.4
						}
					}
				]
				
				MouseArea
				{
					id: mouseArea
					anchors.fill:	parent
					drag.target:	parent
					hoverEnabled:	true
					cursorShape:	Qt.PointingHandCursor
					
					onDoubleClicked:
					{
						if (itemRectangle.draggable)
						{
							listView.clearSelectedItems(true); // Must be before itemDoubleClicked: listView does not exist anymore afterwards
							itemDoubleClicked(index);
						}
					}
					
					onClicked:
					{
						if (itemRectangle.clearOtherSelectedItemsWhenClicked)
						{
							listView.clearSelectedItems(false)
							listView.addSelectedItem(itemRectangle.rank)
						}
					}
					
					onPressed:
					{
						listView.mousePressed = true
						listView.currentIndex = index;
						itemRectangle.clearOtherSelectedItemsWhenClicked = false
						if (mouse.modifiers & Qt.ControlModifier)
						{
							if (itemRectangle.selected)
								listView.removeSelectedItem(itemRectangle.rank)
							else
								listView.addSelectedItem(itemRectangle.rank)
							listView.startShiftSelected = index
							listView.endShiftSelected = -1
						}
						else if (mouse.modifiers & Qt.ShiftModifier)
						{
							if (listView.endShiftSelected >= 0)
								listView.selectShiftItems(false)
							listView.endShiftSelected = index
							listView.selectShiftItems(true)
						}
						else
						{
							itemWrapper.forceActiveFocus()
							if (!itemRectangle.selected)
							{
								listView.clearSelectedItems(false);
								listView.addSelectedItem(itemRectangle.rank);
							}
							else
							{
								itemRectangle.clearOtherSelectedItemsWhenClicked = true;
							}
							
							listView.startShiftSelected = index;
							listView.endShiftSelected = -1;
						}
					}
					onReleased:
					{
						listView.mousePressed = false;
					}
					
					drag.onActiveChanged:
					{
						if (drag.active)
						{
							if (itemRectangle.selected)
							{
								listView.draggingItems = []
								listView.draggingItems.push(itemRectangle)
								itemRectangle.dragging = true;

								var items = listView.getExistingItems();
								for (var i = 0; i < items.length; i++)
								{
									var item = items[i];
									if (!listView.selectedItems.includes(item.rank))
										continue;

									if (item.rank !== index)
									{
										listView.draggingItems.push(item)
										item.dragging = true;
										item.offsetX = item.x - itemRectangle.x;
										item.offsetY = item.y - itemRectangle.y;
										item.setRelative(itemRectangle);
									}
								}
							}
							
						}
						else
						{
							for (var i = 0; i < listView.draggingItems.length; i++)
							{
								var draggingItem = listView.draggingItems[i];
								if (!draggingItem.dragging)
									continue;

								draggingItem.dragging = false;
								draggingItem.x = draggingItem.x; // break bindings
								draggingItem.y = draggingItem.y;
							}
							if (itemRectangle.Drag.target)
							{
								var dropTarget = itemRectangle.Drag.target.parent
								if (dropTarget.singleVariable && listView.selectedItems.length > 1)
									return;
								
								var variablesListName = variablesList.name
								var assignOption = dropTarget.interactionControl ? dropTarget.interactionControl.model.get(dropTarget.interactionControl.currentIndex).value : ""
								itemsDropped(listView.selectedItems, dropTarget, dropTarget.indexInDroppedListViewOfDraggedItem, assignOption);
								listView.clearSelectedItems(true);
							}
						}
					}
				}
			}
		}
	}

	Component
	{
		id: extraCheckBoxComponent

		CheckBox
		{
			id:			extraCheckBoxControl
			name:		extraControlName
			checked:	extraControlProperties["checked"]

			property string controlColName: extraControlColName
			property var	controlModel:	extraControlModel

			Component.onCompleted:		controlModel.controlLoaded(name, extraCheckBoxControl)
			Component.onDestruction:	controlModel.controlDestroyed(name, extraCheckBoxControl)
		}
	}

	Component
	{
		id: extraComboBoxComponent

		ComboBox
		{
			id:				extraComboxControl
			name:			extraControlName
			values:			extraControlProperties["values"]
			currentIndex:	extraControlProperties["currentIndex"]
			control.implicitHeight:	extraControlHeight * 9/10

			property string controlColName:	extraControlColName
			property var	controlModel:	extraControlModel

			Component.onCompleted:		controlModel.controlLoaded(name, extraComboxControl)
			Component.onDestruction:	controlModel.controlDestroyed(name, extraComboxControl)
		}
	}

	Component
	{
		id: extraTextFieldComponent

		TextField
		{
			id:				extraTextFieldControl
			name:			extraControlName
			defaultValue:	extraControlProperties["defaultValue"]

			property string controlColName: extraControlColName
			property var	controlModel:	extraControlModel

			Component.onCompleted:		controlModel.controlLoaded(name, extraTextFieldControl)
			Component.onDestruction:	controlModel.controlDestroyed(name, extraTextFieldControl)
		}
	}

	Component
	{
		id: extraIntegerFieldComponent

		IntegerField
		{
			id:				extraIntegerFieldControl
			name:			extraControlName
			defaultValue:	extraControlProperties["defaultValue"]
			min:			extraControlProperties["min"]
			max:			extraControlProperties["max"]

			property string controlColName: extraControlColName
			property var	controlModel:	extraControlModel

			Component.onCompleted:		controlModel.controlLoaded(name, extraIntegerFieldControl)
			Component.onDestruction:	controlModel.controlDestroyed(name, extraIntegerFieldControl)
		}
	}

}
