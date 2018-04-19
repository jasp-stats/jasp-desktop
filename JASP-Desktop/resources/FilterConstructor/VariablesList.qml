import QtQuick 2.8
import QtQuick.Controls 2.2
import QtQml.Models 2.2

FocusScope
{
	id: variablesList
	width: root.width / 3; height: 300

	property var model;
	property string variablesListType;
	property string dragKey;
	property string dropKey;
	property string defautlDropKey;
	property bool singleItem: false;
	property bool hasSelectedItems: false; // Binding does not work on array length: listView.selectedItems.length > 0;

	readonly property string nominalIconFile: "variable-nominal-inactive.svg"
	readonly property string ordinalIconFile: "variable-ordinal-inactive.svg"
	readonly property string scaleIconFile: "variable-scale-inactive.svg"
	readonly property string iconsFolder: "qrc:/icons/"
	property var suggestedColumns: []
	property var allowedColumns: []

	signal itemDoubleClicked(int index, var dragList);
	signal itemsDropped(var indexes, var dragList, var dropList);

	function selectedItemsChanged() {
		hasSelectedItems = (listView.selectedItems.length > 0);
	}

	function moveSelectedItems(target) {
		console.log('move');
		if (!hasSelectedItems) {
			console.log('no item selected');
			return;
		}

		var selectedIndexes = [];
		for (var i = 0; i < listView.selectedItems.length; i++) {
			var selectedItem = listView.selectedItems[i];
			selectedIndexes.push(selectedItem.rank);
		}

		itemsDropped(selectedIndexes, variablesList, target.getVariablesList());
		listView.clearSelectedItems();
	}

	function getVariablesList() {
		return variablesList;
	}


	DropArea {
		id: dropArea
		anchors.fill: parent
		keys: [dropKey]
	}

	Rectangle {
		id: rectangle
		anchors.fill: parent
		color: "black"
		border.width: 1
		border.color: {
			if (dropArea.containsDrag) return  "red";
			if (variablesList.activeFocus) return "blue";
			return "yellow";
		}

		states: [
			State {
				when: dropArea.containsDrag || variablesList.activeFocus
				PropertyChanges {
					target: rectangle
					border.width: 4
					radius: 3
				}
			}
		]

		transitions: [
			Transition {
				NumberAnimation { properties: "border.width"; duration: 800; easing.type: Easing.OutElastic; easing.amplitude: 1.5;}
			}
		]

		Image {
			id: nominalIcon
			//source: iconsFolder + nominalIconFile
			visible: false
			height: 16
			width: 16
			z: 2
		}
		Image {
			id: ordinalIcon
			//source: iconsFolder + ordinalIconFile
			visible: false
			height: 16
			width: 16
			z: 2
		}
		Image {
			id: scaleIcon
			//source: iconsFolder + scaleIconFile
			visible: false
			height: 16
			width: 16
			z: 2
		}
		Component.onCompleted: {
			for (var i = 0; i < suggestedColumns.length; i++) {
				var icon;
				if (suggestedColumns[i] === "nominal")
					icon = nominalIcon;
				else if (suggestedColumns[i] === "ordinal")
					icon = ordinalIcon;
				else if (suggestedColumns[i] === "scale")
					icon = scaleIcon;
				if (icon) {
					icon.anchors.bottom = rectangle.bottom;
					icon.anchors.bottomMargin = 4
					icon.anchors.right = rectangle.right;
					icon.anchors.rightMargin = i * 20 + 4
					icon.visible = true;
				}
			}
		}

		ListView {
			id: listView
			ScrollBar.vertical: ScrollBar {
				policy: ScrollBar.AsNeeded
			}
			clip: true
			anchors.fill: parent
			anchors.margins: 4
			model: [1, 2, 3, 4, 5, 6, 7]//variablesList.model
			delegate: itemComponent

			property int startShiftSelected: 0;
			property int endShiftSelected: -1;
			property var selectedItems: [];


			function addSelectedItem(item) {
				item.selected = true;
				if (selectedItems.find(function(elt) {return elt.rank === item.rank}))
					return;

				var added = false;
				for (var i = 0; i < selectedItems.length; i++) {
					if (item.rank < selectedItems[i].rank) {
						selectedItems.splice(i, 0, item);
						added = true;
						break;
					}
				}
				if (!added)
					selectedItems.push(item);
				console.log("selected item added");
				variablesList.selectedItemsChanged();
			}

			function removeSelectedItem(item) {
				item.selected = false;
				for (var i = 0; i < selectedItems.length; i++) {
					if (item.rank === selectedItems[i].rank) {
						selectedItems.splice(i, 1);
						break;
					}
				}
				variablesList.selectedItemsChanged();
			}

			function selectItem(item, selected) {
				if (selected)
					listView.addSelectedItem(item);
				else
					listView.removeSelectedItem(item);
			}

			function clearSelectedItems() {
				for (var i = 0; i < selectedItems.length; i++) {
					selectedItems[i].selected = false;
				}
				selectedItems = [];
				variablesList.selectedItemsChanged();
			}

			function selectShiftItems(selected) {
				var startIndex = listView.startShiftSelected;
				var endIndex = listView.endShiftSelected;
				if (startIndex > endIndex) {
					var temp = startIndex;
					startIndex = endIndex;
					endIndex = temp;
				}
				for (var i = startIndex; i <= endIndex; i++) {
					listView.selectItem(listView.contentItem.children[i].children[0], selected);
				}
			}
		}
	}

	Component {
		id: itemComponent
		Item {
			id: itemWrapper
			height: 20;
			width: listView.width
			Rectangle {
				id: itemRectangle
				anchors.horizontalCenter: parent.horizontalCenter
				anchors.verticalCenter: parent.verticalCenter
				height: 20;
				width: listView.width
				focus: true

				property bool selected: false
				property bool dragging: false
				property bool clearOtherSelectedItemsWhenClicked: false
				property int offsetX: 0
				property int offsetY: 0
				property int rank: index

				function setRelative(draggedRect) {
					x = Qt.binding(function (){ return draggedRect.x + offsetX; })
					y = Qt.binding(function (){ return draggedRect.y + offsetY; })
				}


				color: {
					if (itemRectangle.selected)
						return variablesList.activeFocus ? Theme.itemSelectedColor: Theme.itemSelectedNoFocusColor;
					else if (mouseArea.containsMouse)
						return Theme.itemHoverColor;
					return Theme.controlBackgroundColor;
				}
				Drag.keys: [dragKey]
				Drag.active: mouseArea.drag.active
				Drag.hotSpot.x: itemRectangle.width / 2
				Drag.hotSpot.y: itemRectangle.height / 2

				Image {
					id: icon
					height: 15; width: 15
					anchors.verticalCenter: parent.verticalCenter
					source: model.type
				}
				Text {
					id: colName
					x: 20
					text: model.name
					anchors.verticalCenter: parent.verticalCenter
					color: itemRectangle.color === Theme.itemSelectedColor ? Theme.white : Theme.black
				}

				states: [
					State {
						when: itemRectangle.dragging
						ParentChange {
							target: itemRectangle
							parent: root
						}
						AnchorChanges {
							target: itemRectangle
							anchors.horizontalCenter: undefined
							anchors.verticalCenter: undefined
						}
					}
				]

				MouseArea {
					id: mouseArea
					anchors.fill: parent
					drag.target: parent
					hoverEnabled: true

					onDoubleClicked: {
						console.log("onDoubleClicked");
						listView.clearSelectedItems(); // Must be before itemDoubleClicked: listView does not exist anymore afterwards
						itemDoubleClicked(index, variablesList);
					}

					onClicked: {
						console.log("onClicked");
						itemWrapper.forceActiveFocus();
						if (itemRectangle.clearOtherSelectedItemsWhenClicked) {
							listView.clearSelectedItems();
							listView.selectItem(itemRectangle, true);
						}
					}

					onPressed: {
						console.log("onPressed");
						itemRectangle.clearOtherSelectedItemsWhenClicked = false;
						if (mouse.modifiers & Qt.ControlModifier) {
							listView.selectItem(itemRectangle, !itemRectangle.selected);
							listView.startShiftSelected = index;
							listView.endShiftSelected = -1;
						} else if (mouse.modifiers & Qt.ShiftModifier) {
							if (listView.endShiftSelected >= 0)
								listView.selectShiftItems(false)
							listView.endShiftSelected = index;
							listView.selectShiftItems(true);
						} else {
							if (!itemRectangle.selected) {
								listView.clearSelectedItems();
								listView.selectItem(itemRectangle, true);
							} else {
								itemRectangle.clearOtherSelectedItemsWhenClicked = true;
							}

							listView.startShiftSelected = index;
							listView.endShiftSelected = -1;
						}
					}

					drag.onActiveChanged: {
						console.log("drag.onActiveChanged");
						if (drag.active) {
							if (itemRectangle.selected) {
								itemRectangle.dragging = true;
								for (var i = 0; i < listView.selectedItems.length; i++) {
									var selectedItem = listView.selectedItems[i];
									if (selectedItem.rank !== index) {
										selectedItem.dragging = true;
										selectedItem.offsetX = selectedItem.x - itemRectangle.x;
										selectedItem.offsetY = selectedItem.y - itemRectangle.y;
										selectedItem.setRelative(itemRectangle);
									}
								}
							}

						} else {
							var selectedIndexes = [];
							for (var i = 0; i < listView.selectedItems.length; i++) {
								var selectedItem = listView.selectedItems[i];
								selectedIndexes.push(selectedItem.rank);
								console.log("dropped item " + selectedItem.rank);
								selectedItem.dragging = false;
								selectedItem.x = selectedItem.x; // break bindings
								selectedItem.y = selectedItem.y;
							}
							if (itemRectangle.Drag.target) {
								var dropTarget = itemRectangle.Drag.target.parent
								if (dropTarget.singleItem && listView.selectedItems.length > 1)
									return;
								listView.clearSelectedItems(); // Must be before itemsDropped: listView does not exist anymore afterwards
								itemsDropped(selectedIndexes, variablesList, dropTarget);
								console.log("items dropped");
							}
						}
					}
				}
			}
		}
	}
}
