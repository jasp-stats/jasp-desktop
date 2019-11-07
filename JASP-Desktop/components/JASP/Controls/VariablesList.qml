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

import JASP.Widgets 1.0
import QtQuick.Layouts 1.3

JASPControl
{
	id:						variablesList
	controlType:			"VariablesListView"
	background:				variablesListRectangle
	width:					parent.width
	implicitWidth:			width
	height:					singleVariable ? jaspTheme.defaultSingleItemListHeight : jaspTheme.defaultVariablesFormHeight
	implicitHeight:			height
	useControlMouseArea:	false
	
	property var	model
	property string title
	property alias	label:								variablesList.title
	property alias	count:								listView.count
	property var	source
	property int	columns:							1
	property string itemType:							"variables"
	property alias	dropKeys:							dropArea.keys
	property string	dropMode:							"None"
	property bool	draggable:							true
	property alias	syncModels:							variablesList.source
	property bool	showSortMenu:						true
	property string listViewType:						"AvailableVariables"
	property var	sortMenuModel:						null
	property alias	selectedItems:						listView.selectedItems
	property var	allowedColumns:						[]
	property bool	dropModeInsert:						dropMode === "Insert"
	property bool	singleVariable:						false
	property bool	setWidthInForm:						false
	property bool	setHeightInForm:					false
	property bool	dropModeReplace:					dropMode === "Replace"
	property var	suggestedColumns:					[]
	property bool	showElementBorder:					false
	property bool	dragOnlyVariables:					false
	property var	selectedItemsTypes:					[]
	property bool	showVariableTypeIcon:				true
	property bool	mustContainLowerTerms:				true
	property bool	addAvailableVariablesToAssigned:	listViewType === "Interaction"
	
	property var	interactionControl
	property bool	addInteractionOptions:				false
	
	property var	extraControlColumns:				[]
	property var	extraControlComponents:				[]
	property string extraControlOptionName:				""
	property alias	extraControlTitles:					titles.model
	
	property int	indexInDroppedListViewOfDraggedItem:	-1
	
	readonly property int rectangleY: variablesListRectangle.y
	
	signal itemDoubleClicked(int index);
	signal itemsDropped(var indexes, var dropList, int dropItemIndex, string assignOption);
	signal haveSelectedItemsChanged();
	signal draggingChanged(var context, bool dragging);

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

		haveSelectedItemsChanged();
	}

	function setEnabledState(source, dragging)
	{
		var result = !dragging;
		if (dragging)
		{
			if (source.selectedItems.length > 0)
			{
				if (variablesList.allowedColumns.length > 0)
				{
					result = true;
					for (var i = 0; i < source.selectedItemsTypes.length; i++)
					{
						var itemType = source.selectedItemsTypes[i];
						if (!variablesList.allowedColumns.includes(itemType))
							result = false;
					}
				}
				else
					result = true;
			}
		}

		// Do not use variablesList.enabled: this may break the binding if the developer used it in his QML form.
		variablesListRectangle.enabled = result
		variablesListTitle.enabled = result
	}


	function moveSelectedItems(target)
	{
		if (listView.selectedItems.length === 0) return;

		var assignOption = (target && target.interactionControl) ? target.interactionControl.model.get(target.interactionControl.currentIndex).value : ""
		itemsDropped(selectedItems, target, -1, assignOption);
		listView.clearSelectedItems(true);
	}

	Text
	{
		id:				variablesListTitle
		anchors.top:	parent.top
		anchors.left:	parent.left
		text:			title
		height:			title ? jaspTheme.variablesListTitle : 0
		font:			jaspTheme.font
		color:			enabled ? jaspTheme.textEnabled : jaspTheme.textDisabled
		
	}
	
	Row
	{
		width:				parent.width
		anchors.top:		variablesList.top;
		spacing:			5
		layoutDirection:	Qt.RightToLeft
		Repeater
		{
			id: titles;
			Label { text: modelData }
		}
	}
	
	Rectangle
	{
		id:				variablesListRectangle
		anchors.top:	variablesListTitle.bottom
		anchors.left:	parent.left
		height:			variablesList.height - variablesListTitle.height
		width:			parent.width
		color:			debug ? jaspTheme.debugBackgroundColor : jaspTheme.controlBackgroundColor
		border.width:	1
		border.color:	jaspTheme.borderColor
		
		Repeater
		{
			model: suggestedColumns


			Image
			{
				source: jaspTheme.iconPath + (enabled ? iconInactiveFiles[suggestedColumns[index]] : iconDisabledFiles[suggestedColumns[index]])
				height: 16 * preferencesModel.uiScale
				width:	16 * preferencesModel.uiScale
				z:		2
				anchors
				{
					bottom:			variablesListRectangle.bottom;
					bottomMargin:	4  * preferencesModel.uiScale
					right:			variablesListRectangle.right;
					rightMargin:	(index * 20 + 4)  * preferencesModel.uiScale + (scrollBar.visible ? scrollBar.width : 0)
				}
			}
		}

		DropArea
		{
			id:				dropArea
			anchors.fill:	parent

			onPositionChanged:
			{
				if (variablesList.singleVariable || (!variablesList.dropModeInsert && !variablesList.dropModeReplace)) return;

				var onTop = true;
				var item = listView.itemAt(drag.x, drag.y + listView.contentY)
				if (item && item.children.length > 0)
					item = item.children[0];
				if (!item || item.objectName !== "itemRectangle")
				{
					if (listView.count > 0)
					{
						var items = listView.getExistingItems();
						if (items.length > 0)
						{
							var lastItem = items[items.length - 1];
							if (lastItem.rank === (listView.count - 1) && drag.y > (lastItem.height * listView.count))
							{
								item = lastItem
								onTop = false;
							}
						}
					}
				}
				if (item && item.objectName === "itemRectangle")
				{
					dropLine.parent = item
					dropLine.visible = true
					dropLine.onTop = onTop
					listView.itemContainingDrag = item
					variablesList.indexInDroppedListViewOfDraggedItem = onTop ? item.rank : -1
				}
				else
				{
					dropLine.visible = false
					listView.itemContainingDrag = null
					variablesList.indexInDroppedListViewOfDraggedItem = -1
				}
			}
			onExited:
			{
				dropLine.visible = false
				listView.itemContainingDrag = null
				variablesList.indexInDroppedListViewOfDraggedItem = -1
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
				{
					if (!column.name)
						form.addError(qsTr("An ExtraControlColumn in VariablesList %1 has no name defined").arg(variablesList.name))
					if (column.type)
					{
						var type = column.type
						if (type === "DropDown") type = "ComboBox"
						var component = Qt.createComponent(type + ".qml")
						if (component.status === Component.Error)
							form.addError(qsTr("An ExtraControlColumn in VariablesList %1 has an unknown type: %2").arg(variablesList.name).arg(type))
						else
						{
							variablesList.extraControlComponents.push(component)
							variablesList.extraControlColumns.push(column);
						}
					}
					else
						form.addError(qsTr("An ExtraControlColumn in VariablesList %1 has no type defined").arg(variablesList.name))
				}
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

		Rectangle
		{
			id:				dropLine
			height:			1
			width:			parent ? parent.width : 0
			anchors.top:	parent ? (onTop ? parent.top : parent.bottom) : undefined
			anchors.left:	parent ? parent.left : undefined
			color:			jaspTheme.blueLighter
			visible:		false

			property bool onTop: true
		}

		SortMenuButton
		{
			visible: variablesList.showSortMenu && variablesList.sortMenuModel && listView.count > 1
			anchors
			{
				top:			parent.top
				right:			parent.right
				rightMargin:	5 * preferencesModel.uiScale + (scrollBar.visible ? scrollBar.width : 0)
				topMargin:		5 * preferencesModel.uiScale
			}

			sortMenuModel:		variablesList.sortMenuModel
			scrollYPosition:	backgroundForms.contentY
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
			
			property int	startShiftSelected:	0
			property int	endShiftSelected:	-1
			property var	selectedItems:		[]
			property bool	mousePressed:		false
			property bool	shiftPressed:		false
			property var	draggingItems:		[]
			property var	itemContainingDrag
			
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
			
			Component.onDestruction:
			{
				if (itemRectangle.extraColumnsModel)
					itemRectangle.extraColumnsModel.controlsDestroyed()
			}

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
				border.color:	containsDragItem && variablesList.dropModeReplace ? jaspTheme.containsDragBorderColor : jaspTheme.grayLighter
				
				
				property bool clearOtherSelectedItemsWhenClicked: false
				property bool selected:				listView.selectedItems.includes(rank)
				property bool isDependency:			variablesList.dependencyMustContain.indexOf(colName.text) >= 0
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
					if(itemRectangle.isDependency)											return itemRectangle.selected ? jaspTheme.dependencySelectedColor : jaspTheme.dependencyBorderColor;
					if (!itemRectangle.draggable)											return jaspTheme.controlBackgroundColor;
					if (itemRectangle.selected)												return variablesList.activeFocus ? jaspTheme.itemSelectedColor: jaspTheme.itemSelectedNoFocusColor;
					if (itemRectangle.containsDragItem && variablesList.dropModeReplace)	return jaspTheme.itemSelectedColor;
					if (mouseArea.containsMouse)											return jaspTheme.itemHoverColor;

					return jaspTheme.controlBackgroundColor;
				}

				Drag.keys:		[variablesList.name]
				Drag.active:	mouseArea.drag.active
				Drag.hotSpot.x:	itemRectangle.width / 2
				Drag.hotSpot.y:	itemRectangle.height / 2
				
				// Use the ToolTip Attached property to avoid creating ToolTip object for each item
				QTCONTROLS.ToolTip.visible: mouseArea.containsMouse && model.name && !itemRectangle.containsDragItem && colName.truncated
				QTCONTROLS.ToolTip.delay: 300
				QTCONTROLS.ToolTip.text: model.name
				
				Image
				{
					id:						icon
					height:					15 * preferencesModel.uiScale
					width:					15 * preferencesModel.uiScale
					anchors.verticalCenter:	parent.verticalCenter
					source:					(!(variablesList.showVariableTypeIcon && itemRectangle.isVariable) || !model.columnType) ? "" : jaspTheme.iconPath + (enabled ? iconFiles[model.columnType] : iconDisabledFiles[model.columnType])
					visible:				variablesList.showVariableTypeIcon && itemRectangle.isVariable
				}
				Text
				{
					id:						colName
					x:						(variablesList.showVariableTypeIcon ? 20 : 4) * preferencesModel.uiScale
					text:					model.name
					width:					itemRectangle.width - x - extraControls.width
					elide:					Text.ElideRight
					anchors.verticalCenter:	parent.verticalCenter
					horizontalAlignment:	itemRectangle.isLayer ? Text.AlignHCenter : undefined
					color:					!enabled ? jaspTheme.textDisabled : itemRectangle.isVirtual ? jaspTheme.grayLighter : (itemRectangle.color === jaspTheme.itemSelectedColor ? jaspTheme.white : jaspTheme.black)
					font:					jaspTheme.font
				}
				
				RowLayout
				{
					id:						extraControls
					anchors.verticalCenter:	parent.verticalCenter
					anchors.right:			parent.right
					spacing:				1
					z:						10
					
					layoutDirection: Qt.RightToLeft
					
					Repeater
					{
						model: itemRectangle.extraColumnsModel
						
						delegate: Loader
						{
							sourceComponent: variablesList.extraControlComponents[index]
							asynchronous:	false

							onLoaded:
							{
								item.name = model.name
								var keys = Object.keys(model.properties)
								var values = Object.values(model.properties)
								for (var i = 0; i < keys.length; i++) {
									var name = keys[i]
									if (item.hasOwnProperty(name))
										item[name] = values[i]
									else if (name === "rightMargin")
										Layout.rightMargin = values[i]
								}
								itemRectangle.extraColumnsModel.controlLoaded(item.name, item)
							}

							onStatusChanged: if (status === Loader.Error) form.addError(qsTr("Error when trying to load control: %1").arg(sourceComponent.errorString()))
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
					id:				mouseArea
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
						variablesList.draggingChanged(variablesList, drag.active)
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
								var dropTarget = itemRectangle.Drag.target.parent.parent
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

}
