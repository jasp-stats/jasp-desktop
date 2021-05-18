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


import QtQuick			2.11
import QtQuick.Controls 2.4 as QTCONTROLS
import QtQml.Models		2.2
import JASP.Widgets		1.0
import JASP				1.0

VariablesListBase
{
	id						: variablesList
	implicitHeight			: maxRows === 1 ? jaspTheme.defaultSingleItemListHeight : jaspTheme.defaultVariablesFormHeight
	background				: itemRectangle
	implicitWidth 			: parent.width
	shouldStealHover		: false
	innerControl			: itemGridView
	optionKey				: listViewType === JASP.Interaction ? "components" : "variable"

	property alias	label							: variablesList.title
	property alias	itemGridView					: itemGridView
	property alias	cellHeight						: itemGridView.cellHeight
	property alias	cellWidth						: itemGridView.cellWidth
	property alias	itemRectangle					: itemRectangle
	property alias	scrollBar						: scrollBar
	property alias	itemTitle						: itemTitle
	property string	rowComponentTitle				: ""
	property string itemType						: "variables"
	property int	dropMode						: JASP.DropNone
	property bool	draggable						: true
	property var	sortMenuModel					: null
	property bool	showSortMenu					: true
	property bool	singleVariable					: false
	property int	maxRows							: (singleVariable ? 1 : -1)
	property bool	dropModeInsert					: dropMode === JASP.DropInsert
	property bool	dropModeReplace					: dropMode === JASP.DropReplace
	property bool	showElementBorder				: false
	property bool	showVariableTypeIcon			: containsVariables
	property bool	addInteractionsByDefault		: true
	property bool	interactionContainLowerTerms	: true
	property bool	addAvailableVariablesToAssigned	: listViewType === JASP.Interaction
	property bool	allowAnalysisOwnComputedColumns	: true
	property bool	allowDuplicatesInMultipleColumns: false // This property is used in the constructor and is not updatable afterwards.

	property var	interactionControl
	property bool	addInteractionOptions			: false

	property int	indexInDroppedListViewOfDraggedItem:	-1
	
	readonly property int rectangleY				: itemRectangle.y

	property int	startShiftSelected				: 0
	property int	endShiftSelected				: -1
	property bool	mousePressed					: false
	property bool	shiftPressed					: false
	property var	draggingItems					: []
	property var	itemContainingDrag
	property string	searchKeys						: ""
	
	signal itemDoubleClicked(int index);
	signal itemsDropped(var indexes, var dropList, int dropItemIndex, int assignOption);
	signal draggingChanged(var context, bool dragging);
	signal selectedItemsChanged();

	onModelChanged: if (model) model.selectedItemsChanged.connect(selectedItemsChanged);

	function setEnabledState(source, dragging)
	{
		var result = !dragging;
		if (dragging)
		{
			if (source.model.selectedItems().length > 0)
			{
				if (variablesList.allowedColumns.length > 0)
				{
					result = false;
					var sourceSelectedItemsTypes = source.model.selectedItemsTypes()
					for (var i = 0; i < sourceSelectedItemsTypes.length; i++)
					{
						var itemType = sourceSelectedItemsTypes[i];
						if (variablesList.allowedColumns.includes(itemType))
							result = true;
					}
				}
				else
					result = true;
			}
		}

		// Do not use variablesList.enabled: this may break the binding if the developer used it in his QML form.
		itemRectangle.enabled = result
		itemTitle.enabled = result
	}


	function moveSelectedItems(target)
	{
		var selectedItems = variablesList.model.selectedItems()
		if (selectedItems.length === 0) return;

		var assignOption = (target && target.interactionControl) ? JASP.AssignCross: JASP.AssignDefault
		itemsDropped(selectedItems, target, -1, assignOption);
		variablesList.clearSelectedItems();
	}



	function getExistingItems()
	{
		var items = [];
		for (var i = 0; i < itemGridView.contentItem.children.length; i++)
		{
			var item = itemGridView.contentItem.children[i];
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
		variablesList.model.selectItem(itemRank, true);
	}

	function removeSelectedItem(itemRank)
	{
		variablesList.model.selectItem(itemRank, false);
	}

	function clearSelectedItems()
	{
		variablesList.model.clearSelectedItems();
	}

	function setSelectedItem(itemRank)
	{
		variablesList.model.setSelectedItem(itemRank);
	}

	function selectShiftItems(selected)
	{
		var startIndex = variablesList.startShiftSelected;
		var endIndex = variablesList.endShiftSelected;
		if (startIndex > endIndex)
		{
			var temp = startIndex;
			startIndex = endIndex;
			endIndex = temp;
		}

		for (var i = startIndex; i <= endIndex; i++)
			variablesList.model.selectItem(i, selected)
	}

	Text
	{
		id				: itemTitle
		anchors.top		: parent.top
		anchors.left	: parent.left
		text			: title
		height			: title ? jaspTheme.variablesListTitle : 0
		font			: jaspTheme.font
		color			: enabled ? jaspTheme.textEnabled : jaspTheme.textDisabled
	}

	Text
	{
		anchors.top		: parent.top
		anchors.right	: parent.right
		text			: rowComponentTitle
		height			: rowComponentTitle ? jaspTheme.variablesListTitle : 0
		font			: jaspTheme.font
		color			: enabled ? jaspTheme.textEnabled : jaspTheme.textDisabled
	}

	Rectangle
	{
		id				: itemRectangle
		anchors.top		: itemTitle.bottom
		anchors.left	: parent.left
		height			: variablesList.height - itemTitle.height
		width			: parent.width
		color			: debug ? jaspTheme.debugBackgroundColor : jaspTheme.controlBackgroundColor
		border.width	: 1
		border.color	: jaspTheme.borderColor
		radius			: jaspTheme.borderRadius

		JASPScrollBar
		{
			id				: scrollBar
			flickable		: itemGridView
			manualAnchor	: true
			vertical		: true
			z				: 1337

			anchors
			{
				top			: parent.top
				right		: parent.right
				bottom		: parent.bottom
				margins		: 2
			}
		}

		GridView
		{
			id						: itemGridView
			cellHeight				: 20  * preferencesModel.uiScale
			cellWidth				: width / variablesList.columns
			clip					: true
			focus					: true
			anchors.fill			: parent
			anchors.margins			: 4 * preferencesModel.uiScale
			anchors.rightMargin		: scrollBar.width + anchors.margins
			model					: variablesList.model
			delegate				: itemVariableComponent
			boundsBehavior			: Flickable.StopAtBounds

			onCurrentItemChanged:
			{
				if (variablesList.shiftPressed)
				{
					if (variablesList.endShiftSelected >= 0)
						variablesList.selectShiftItems(false);
					variablesList.endShiftSelected = itemGridView.currentIndex;
					variablesList.selectShiftItems(true);
				}
				else if (!variablesList.mousePressed)
				{
					var itemWrapper = currentItem;
					if (itemWrapper)
					{
						var itemRectangle = itemWrapper.children[0];
						variablesList.setSelectedItem(itemRectangle.rank);
						variablesList.startShiftSelected = currentIndex;
						variablesList.endShiftSelected = -1;
					}
				}
			}
		}
	}
		
	Repeater
	{
		model: suggestedColumnsIcons

		Image
		{
			source: modelData
			height: 16 * preferencesModel.uiScale
			width:	16 * preferencesModel.uiScale
			z:		2
			mipmap:	true
			smooth:	true
			anchors
			{
				bottom:			itemRectangle.bottom;
				bottomMargin:	4  * preferencesModel.uiScale
				right:			itemRectangle.right;
				rightMargin:	(index * 20 + 4)  * preferencesModel.uiScale + (scrollBar.visible ? scrollBar.width : 0)
			}
		}
	}

	DropArea
	{
		id:				dropArea
		anchors.fill:	itemRectangle
		keys:			variablesList.dropKeys

		onPositionChanged:
		{
			if (!itemRectangle.enabled || variablesList.maxRows === 1 || (!variablesList.dropModeInsert && !variablesList.dropModeReplace)) return;

			var onTop = true;
			var item = itemGridView.itemAt(drag.x, drag.y + itemGridView.contentY)
			if (item && item.children.length > 0)
				item = item.children[0];
			if (!item || item.objectName !== "itemRectangle")
			{
				if (itemGridView.count > 0)
				{
					var items = variablesList.getExistingItems();
					if (items.length > 0)
					{
						var lastItem = items[items.length - 1];
						if (lastItem.rank === (itemGridView.count - 1) && drag.y > (lastItem.height * itemGridView.count))
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
				variablesList.itemContainingDrag = item
				variablesList.indexInDroppedListViewOfDraggedItem = onTop ? item.rank : -1
			}
			else
			{
				dropLine.visible = false
				variablesList.itemContainingDrag = null
				variablesList.indexInDroppedListViewOfDraggedItem = -1
			}
		}
		onExited:
		{
			dropLine.visible = false
			variablesList.itemContainingDrag = null
			variablesList.indexInDroppedListViewOfDraggedItem = -1
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
		visible: variablesList.showSortMenu && variablesList.sortMenuModel && itemGridView.count > 1
		anchors
		{
			top:			itemRectangle.top
			right:			itemRectangle.right
			rightMargin:	5 * preferencesModel.uiScale + (scrollBar.visible ? scrollBar.width : 0)
			topMargin:		5 * preferencesModel.uiScale
		}

		sortMenuModel: variablesList.sortMenuModel
		scrollYPosition: backgroundForms.contentY
	}
			
	Timer
	{
		id: searchKeysTimer
		interval: 500
		onTriggered: variablesList.searchKeys = ""
	}

	Keys.onPressed:
	{
		if (event.key === Qt.Key_Shift)
			variablesList.shiftPressed = true;
		else if (event.key === Qt.Key_A && event.modifiers & Qt.ControlModifier)
			variablesList.model.selectAllItems();
		else if (event.key >= Qt.Key_Exclam && event.key <= Qt.Key_ydiaeresis)
		{
			var currentSearchKeys = variablesList.searchKeys
			var stringToSearch = (currentSearchKeys.length === 1 && currentSearchKeys === event.text) ? event.text : currentSearchKeys + event.text
			var nextIndex = variablesList.model.searchTermWith(stringToSearch)

			if (nextIndex >= 0)
			{
				itemGridView.positionViewAtIndex(nextIndex, GridView.Contain)
				if (itemGridView.currentIndex !== nextIndex)
				{
					variablesList.clearSelectedItems();
					itemGridView.currentIndex = nextIndex;
				}
				searchKeysTimer.restart()
				variablesList.searchKeys = stringToSearch
			}
		}
	}

	Keys.onReleased:
	{
		if (event.key === Qt.Key_Shift)
			variablesList.shiftPressed = false;
	}

	Keys.onSpacePressed:
	{
		moveSelectedItems()
	}
	Keys.onReturnPressed:
	{
		moveSelectedItems()
	}
	
	Component
	{
		id: itemVariableComponent

		FocusScope
		{
			id:			itemWrapper
			height:		itemGridView.cellHeight
			width:		itemGridView.cellWidth
			
			Rectangle
			{
				id:							itemRectangle
				objectName:					"itemRectangle"
				anchors.horizontalCenter:	parent.horizontalCenter
				anchors.verticalCenter:		parent.verticalCenter
				// the height & width of itemWrapper & itemRectangle must be set independently of each other:
				// when the rectangle is dragged, it gets another parent but it must keep the same size,
				height:			itemGridView.cellHeight
				width:			itemGridView.cellWidth
				focus:			true
				border.width:	containsDragItem && variablesList.dropModeReplace ? 2 : (variablesList.showElementBorder ? 1 : 0)
				border.color:	containsDragItem && variablesList.dropModeReplace ? jaspTheme.containsDragBorderColor : jaspTheme.grayLighter
				radius:			jaspTheme.borderRadius
				
				property bool clearOtherSelectedItemsWhenClicked: false
				property bool selected:				model.selected
				property bool isDependency:			variablesList.dependencyMustContain.indexOf(colName.text) >= 0
				property bool dragging:				false
				property int offsetX:				0
				property int offsetY:				0
				property int rank:					index
				property bool containsDragItem:		variablesList.itemContainingDrag === itemRectangle
				property bool isVirtual:			(typeof model.type !== "undefined") && model.type.includes("virtual")
				property bool isVariable:			(typeof model.type !== "undefined") && model.type.includes("variable")
				property bool isLayer:				(typeof model.type !== "undefined") && model.type.includes("layer")
				property bool draggable:			variablesList.draggable && model.selectable
				property string columnType:			isVariable && (typeof model.columnType !== "undefined") ? model.columnType : ""
				property var extraItem:				model.rowComponent

				enabled: variablesList.listViewType != JASP.AvailableVariables || !columnType || variablesList.allowedColumns.length == 0 || (variablesList.allowedColumns.indexOf(columnType) >= 0)
				
				function setRelative(draggedRect)
				{
					x = Qt.binding(function (){ return draggedRect.x + offsetX; })
					y = Qt.binding(function (){ return draggedRect.y + offsetY; })
				}
				
				color:
				{
					if (itemRectangle.isDependency)											return itemRectangle.selected ? jaspTheme.dependencySelectedColor : jaspTheme.dependencyBorderColor;
					if (itemRectangle.draggable)
					{
						if (itemRectangle.selected)												return variablesList.activeFocus ? jaspTheme.itemSelectedColor: jaspTheme.itemSelectedNoFocusColor;
						if (itemRectangle.containsDragItem && variablesList.dropModeReplace)	return jaspTheme.itemSelectedColor;
						if (mouseArea.containsMouse)											return jaspTheme.itemHoverColor;
					}
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
				
				Component.onCompleted:
				{
					if (extraItem)
					{
						extraItem.parent = itemRectangle;
						extraItem.anchors.verticalCenter = itemRectangle.verticalCenter;
						extraItem.anchors.right = itemRectangle.right;
						extraItem.anchors.rightMargin = 3 * preferencesModel.uiScale;
					}
				}

				Image
				{
					id:						icon
					height:					16 * preferencesModel.uiScale
					width:					source === "" ? 0 : 16 * preferencesModel.uiScale
					x:						jaspTheme.borderRadius
					anchors.verticalCenter:	parent.verticalCenter
					source:					sourceVar !== undefined ? sourceVar : ""
					visible:				source
					mipmap:					true
					smooth:					true

					//So Im pushing this through a property because it seems to results in "undefined" during loading and this adds a ton of warnings to the output which is not helpful. I tried less heavyhanded approaches first but this works perfectly fine.
					property var sourceVar:	variablesList.showVariableTypeIcon && itemRectangle.isVariable ? (enabled ? model.columnTypeIcon : model.columnTypeDisabledIcon) : ""
				}

				Text
				{
					id:						colName
					anchors.left:			variablesList.showVariableTypeIcon ? icon.right : itemRectangle.left
					anchors.leftMargin:		jaspTheme.generalAnchorMargin
					text:					model.name
					width:					itemRectangle.width - x - (itemRectangle.extraItem ? itemRectangle.extraItem.width : 0)
					elide:					Text.ElideRight
					anchors.verticalCenter:	parent.verticalCenter
					horizontalAlignment:	itemRectangle.isLayer ? Text.AlignHCenter : undefined
					color:					!enabled ? jaspTheme.textDisabled : itemRectangle.isVirtual ? jaspTheme.grayLighter : (itemRectangle.color === jaspTheme.itemSelectedColor ? jaspTheme.white : jaspTheme.black)
					font:					jaspTheme.font
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
					drag.target:	itemRectangle.draggable ? parent : null
					hoverEnabled:	true
					cursorShape:	Qt.PointingHandCursor
					
					onDoubleClicked:
					{
						if (itemRectangle.draggable)
						{
							variablesList.clearSelectedItems(); // Must be before itemDoubleClicked: listView does not exist anymore afterwards
							itemDoubleClicked(index);
						}
					}
					
					onClicked:
					{
						if (itemRectangle.clearOtherSelectedItemsWhenClicked)
							variablesList.setSelectedItem(itemRectangle.rank)
					}
					
					onPressed:
					{
						variablesList.mousePressed = true
						itemGridView.currentIndex = index;
						itemRectangle.clearOtherSelectedItemsWhenClicked = false
						if (mouse.modifiers & Qt.ControlModifier)
						{
							if (itemRectangle.selected)
								variablesList.removeSelectedItem(itemRectangle.rank)
							else
								variablesList.addSelectedItem(itemRectangle.rank)
							variablesList.startShiftSelected = index
							variablesList.endShiftSelected = -1
						}
						else if (mouse.modifiers & Qt.ShiftModifier)
						{
							if (variablesList.endShiftSelected >= 0)
								variablesList.selectShiftItems(false)
							variablesList.endShiftSelected = index
							variablesList.selectShiftItems(true)
						}
						else
						{
							itemWrapper.forceActiveFocus()
							if (!itemRectangle.selected)
								variablesList.setSelectedItem(itemRectangle.rank);
							else
								itemRectangle.clearOtherSelectedItemsWhenClicked = true;
							
							variablesList.startShiftSelected = index;
							variablesList.endShiftSelected = -1;
						}
					}
					onReleased:
					{
						variablesList.mousePressed = false;
					}
					
					drag.onActiveChanged:
					{
						variablesList.draggingChanged(variablesList, drag.active)
						if (drag.active)
						{
							if (itemRectangle.selected)
							{
								variablesList.draggingItems = []
								variablesList.draggingItems.push(itemRectangle)
								itemRectangle.dragging = true;

								var items = variablesList.getExistingItems();
								for (var i = 0; i < items.length; i++)
								{
									var item = items[i];
									if (!variablesList.model.selectedItems().includes(item.rank))
										continue;

									if (item.rank !== index)
									{
										variablesList.draggingItems.push(item)
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
							for (var i = 0; i < variablesList.draggingItems.length; i++)
							{
								var draggingItem = variablesList.draggingItems[i];
								if (!draggingItem.dragging)
									continue;

								draggingItem.dragging = false;
								draggingItem.x = draggingItem.x; // break bindings
								draggingItem.y = draggingItem.y;
							}
							if (itemRectangle.Drag.target)
							{
								var dropTarget = itemRectangle.Drag.target.parent
								var selectedItems = variablesList.model.selectedItems()
								if (dropTarget.maxRows > 0 && selectedItems.length > dropTarget.maxRows)
									return;
								
								var variablesListName = variablesList.name
								var assignOption = dropTarget.interactionControl ? dropTarget.interactionControl.model.get(dropTarget.interactionControl.currentIndex).value : JASP.AssignDefault
								itemsDropped(selectedItems, dropTarget, dropTarget.indexInDroppedListViewOfDraggedItem, assignOption);
								variablesList.clearSelectedItems();
							}
						}
					}
				}
			}
		}
	}
}
