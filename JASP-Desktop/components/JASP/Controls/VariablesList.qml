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
import QtQuick.Controls 2.4
import QtQml.Models 2.2
import JASP.Theme 1.0

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
	property bool	singleVariable:			false
	property string listViewType:		"AvailableVariables"
	property var	allowedColumns:		[]
	property bool	dropModeInsert:		dropMode === "Insert"
	property bool	dropModeReplace:	dropMode === "Replace"
	property bool	hasSelectedItems:	false; // Binding does not work on array length: listView.selectedItems.length > 0;
	property var	suggestedColumns:	[]
	property bool	showElementBorder:	false
	property bool	dragOnlyVariables:	false
	property bool	showVariableTypeIcon:	true
	property bool	setWidthInForm:		false
	property bool	setHeightInForm:	false
	
	
	property var	extraControlColumns:		[]
	property string extraControlOptionName:		""
	property alias	extraControlTitles:	titles.model
	
	property int	indexInDroppedListViewOfDraggedItem:	-1
	
	readonly property int rectangleY: rectangle.y
	
	signal itemDoubleClicked(int index);
	signal itemsDropped(var indexes, var dropList, int dropItemIndex);
	
	function selectedItemsChanged()
	{
		hasSelectedItems = (listView.selectedItems.length > 0);
	}
	
	function moveSelectedItems(target)
	{
		if (!hasSelectedItems) return;
		
		var selectedIndexes = [];
		for (var i = 0; i < listView.selectedItems.length; i++)
		{
			var selectedItem = listView.selectedItems[i];            
			selectedIndexes.push(selectedItem.rank);
		}
		
		// itemsDropped will change the listView, and that may call the onCurrentItemChanged
		// So we have to clear the selected items list before.
		listView.clearSelectedItems();
		itemsDropped(selectedIndexes, target, -1);
		
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
			model: suggestedColumns.length


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
					rightMargin:	(index * 20 + 4)  * preferencesModel.uiScale
				}
			}


		}
		
		Component.onCompleted:
		{
			if (suggestedColumns.length === 0)
				suggestedColumns = allowedColumns
			
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

			anchors
			{
				top:		parent.top
				right:		parent.right
				bottom:		parent.bottom
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
			
			property int startShiftSelected: 0;
			property int endShiftSelected: -1;
			property var selectedItems: [];
			property bool mousePressed: false;
			property bool shiftPressed: false;
			property var itemContainingDrag
			
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
						itemWrapper.forceActiveFocus();
						listView.clearSelectedItems();
						listView.selectItem(itemRectangle, true);
						listView.startShiftSelected = listView.currentIndex;
						listView.endShiftSelected = -1;
					}
				}
			}
			
			Keys.onPressed:
			{
				if (event.modifiers & Qt.ShiftModifier)
					shiftPressed = true;
				else
					shiftPressed = false;
			}
			
			Keys.onReleased:
			{
				if (event.modifiers & Qt.ShiftModifier)
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
			
			function addSelectedItem(item)
			{
				if (!item || item.objectName !== "itemRectangle")
				{
					console.log("item is not an itemRectangle!!!!")
					return;
				}
				if (!item.draggable)
					return;
				
				item.selected = true;
				if (selectedItems.find(function(elt) {return elt.rank === item.rank}))
					return;
				
				var added = false;
				for (var i = 0; i < selectedItems.length; i++)
				{
					if (item.rank < selectedItems[i].rank)
					{
						selectedItems.splice(i, 0, item);
						added = true;
						break;
					}
				}
				if (!added)
					selectedItems.push(item);
				
				variablesList.selectedItemsChanged();
			}
			
			function removeSelectedItem(item)
			{
				
				if (!item || item.objectName !== "itemRectangle")
					return;
				
				item.selected = false;
				for (var i = 0; i < selectedItems.length; i++)
				{
					if (item.rank === selectedItems[i].rank)
					{
						selectedItems.splice(i, 1);
						break;
					}
				}
				variablesList.selectedItemsChanged();
			}
			
			function selectItem(item, selected)
			{
				if (selected)
					listView.addSelectedItem(item);
				else
					listView.removeSelectedItem(item);
			}        
			
			function clearSelectedItems()
			{
				for (var i = 0; i < selectedItems.length; i++)
				{
					selectedItems[i].selected = false;
				}
				selectedItems = [];
				variablesList.selectedItemsChanged();
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
				for (var i = startIndex; i <= endIndex; i++)
				{
					var item = listView.contentItem.children[i];
					if (item)
						listView.selectItem(item.children[0], selected);
					else
						console.log(variablesList.name + ": Unknown item at index " + i);
				}
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
				property bool selected:				false
				property bool dragging:				false
				property int offsetX:				0
				property int offsetY:				0
				property int rank:					index
				property bool containsDragItem:		listView.itemContainingDrag === itemRectangle
				property bool isVirtual:			(typeof model.type !== "undefined") && model.type.includes("virtual")
				property bool isVariable:			(typeof model.type !== "undefined") && model.type.includes("variable")
				property bool isLayer:				(typeof model.type !== "undefined") && model.type.includes("layer")
				property bool draggable:			!variablesList.dragOnlyVariables || isVariable
				property string columnType:			isVariable ? model.columnType : ""
				property var extraColumnsModel:		model.extraColumns
				
				function setRelative(draggedRect)
				{
					x = Qt.binding(function (){ return draggedRect.x + offsetX; })
					y = Qt.binding(function (){ return draggedRect.y + offsetY; })
				}
				
				color:
				{
					if (!itemRectangle.draggable)												return Theme.controlBackgroundColor;
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
				ToolTip.visible: mouseArea.containsMouse && model.name && !itemRectangle.containsDragItem
				
				ToolTip.delay: 300
				ToolTip.text: model.name
				ToolTip.toolTip.background: Rectangle
				{
					id:		tooltipRectangle
					color:	Theme.tooltipBackgroundColor
				}
				
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
					source:					!(variablesList.showVariableTypeIcon && itemRectangle.isVariable) ? "" : enabled ? iconFiles[model.columnType] : iconDisabledFiles[model.columnType]
					visible:				variablesList.showVariableTypeIcon && itemRectangle.isVariable
				}
				Text
				{
					id:						colName
					x:						variablesList.showVariableTypeIcon ? 20 : 4
					text:					model.name
					width:					itemRectangle.width - x
					elide:					Text.ElideRight
					anchors.verticalCenter:	parent.verticalCenter
					horizontalAlignment:	itemRectangle.isLayer ? Text.AlignHCenter : undefined
					color:					!enabled ? Theme.textDisabled : itemRectangle.isVirtual ? Theme.grayLighter : (itemRectangle.color === Theme.itemSelectedColor ? Theme.white : Theme.black)
					font:					Theme.font
				}
				
				Row
				{
					anchors.fill:			parent
					anchors.rightMargin:	10  * preferencesModel.uiScale
					spacing:				1
					z:						10 * preferencesModel.uiScale
					
					layoutDirection: Qt.RightToLeft
					
					Repeater
					{
						model: itemRectangle.extraColumnsModel
						
						delegate: Loader
						{
							source:			model.path
							asynchronous:	false
							
							onLoaded:		itemRectangle.extraColumnsModel.controlLoaded(model.name, item)
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
						listView.clearSelectedItems(); // Must be before itemDoubleClicked: listView does not exist anymore afterwards
						itemDoubleClicked(index);
					}
					
					onClicked:
					{
						if (itemRectangle.clearOtherSelectedItemsWhenClicked)
						{
							listView.clearSelectedItems();
							listView.selectItem(itemRectangle, true);
						}
					}
					
					onPressed:
					{
						listView.mousePressed = true;
						listView.currentIndex = index;
						itemRectangle.clearOtherSelectedItemsWhenClicked = false;
						if (mouse.modifiers & Qt.ControlModifier)
						{
							listView.selectItem(itemRectangle, !itemRectangle.selected);
							listView.startShiftSelected = index;
							listView.endShiftSelected = -1;
						}
						else if (mouse.modifiers & Qt.ShiftModifier)
						{
							if (listView.endShiftSelected >= 0)
								listView.selectShiftItems(false)
							listView.endShiftSelected = index;
							listView.selectShiftItems(true);
						}
						else
						{
							itemWrapper.forceActiveFocus();
							if (!itemRectangle.selected)
							{
								listView.clearSelectedItems();
								listView.selectItem(itemRectangle, true);
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
								itemRectangle.dragging = true;
								for (var i = 0; i < listView.selectedItems.length; i++)
								{
									var selectedItem = listView.selectedItems[i];
									if (selectedItem.objectName !== "itemRectangle")
									{
										console.log("This is not an itemRectangle!")
										continue;
									}
									
									if (selectedItem.rank !== index)
									{
										selectedItem.dragging = true;
										selectedItem.offsetX = selectedItem.x - itemRectangle.x;
										selectedItem.offsetY = selectedItem.y - itemRectangle.y;
										selectedItem.setRelative(itemRectangle);                                
									}
								}
							}
							
						}
						else
						{
							var selectedIndexes = [];
							for (var i = 0; i < listView.selectedItems.length; i++)
							{
								var selectedItem = listView.selectedItems[i];
								selectedIndexes.push(selectedItem.rank);
								selectedItem.dragging = false;
								selectedItem.x = selectedItem.x; // break bindings
								selectedItem.y = selectedItem.y;
							}
							if (itemRectangle.Drag.target)
							{
								var dropTarget = itemRectangle.Drag.target.parent
								if (dropTarget.singleVariable && listView.selectedItems.length > 1)
									return;                                
								
								listView.clearSelectedItems(); // Must be before itemsDropped: listView does not exist anymore afterwards
								var variablesListName = variablesList.name
								itemsDropped(selectedIndexes, dropTarget, dropTarget.indexInDroppedListViewOfDraggedItem);                               
							}
						}
					}
				}
			}			
		}
	}
	
}
