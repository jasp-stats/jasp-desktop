import QtQuick 2.9
import QtQuick.Controls 2.2



DropArea {
	//Rectangle { color: "transparent"; border.color: "blue"; border.width: 1; anchors.fill: parent } //debug for size of the dropspots

	id: dragTarget
	objectName: "DropSpot"
	property string __debugName: "DropSpot " + (parent !== undefined && parent.__debugName !== undefined ? parent.__debugName : "???")

	property var dropKeys: [ "number", "boolean", "string", "variable" ]
	property alias dropProxy: dragTarget

	width:  implicitWidth
	height: implicitHeight
	keys: ["all"]
	property real originalWidth: defaultText.length * filterConstructor.blockDim * 0.4
	property bool acceptsDrops: true
	property string defaultText: acceptsDrops ? "..." : shouldShowX ? "y" : ""
	property bool droppedShouldBeNested: false
	property bool shouldShowX: false
	property bool iWasChecked: false

	implicitWidth: dropText.contentWidth
	implicitHeight: filterConstructor.blockDim

	property bool beingDragHovered: false
	property color dragHoverColor: jaspTheme.blue

	Rectangle
	{
		id: dragMarker
		z: -3
		visible: containsDrag || beingDragHovered
		radius: width
		anchors.fill: parent
		border.color: dragTarget.dragHoverColor
		border.width: 3
		color: "transparent"
	}

	function checkCompletenessFormulas()
	{
		iWasChecked = true

		//console.log("checkCompletenessFormulas on " + __debugName + " and containsItem: " + ( containsItem === null ? "null" : containsItem.__debugName))

		if(containsItem !== null)
		{
			//if(containsItem.objectName !== "DragGeneric")
				//console.log("problem! ",parent.objectName," doesnt contain a dragger in dropper")
			return containsItem.checkCompletenessFormulas()
		}
		return false
	}

	onEntered:
	{
		//console.log(__debugName," onEntered DROPSPOT")

		if((containsItem !== null && containsItem !== drag.source) || !acceptsDrops)
		{
			drag.accepted = false
			return
		}

		var ancestry = parent
		while(ancestry !== null)
		{
			if((ancestry.objectName === "DragGeneric" && ancestry.dragChild === drag.source) || ancestry === drag.source)
			{
				drag.accepted = false
				return
			}

			ancestry = ancestry.parent
		}

		var foundOneValidDragKey = false
		for(var i=0; i<drag.source.dragKeys.length; i++)
			if(dropKeys.indexOf(drag.source.dragKeys[i]) >= 0)
				foundOneValidDragKey = true

		beingDragHovered = true

		dragHoverColor = foundOneValidDragKey ? jaspTheme.green : jaspTheme.red

		originalWidth = width
		width = drag.source.width
	}

	onExited:
	{
		//console.log(__debugName," onExited DROPSPOT")

		if(containsItem == null)
			width = originalWidth

		beingDragHovered = false
	}

	//onDropped: containsItem = drop.drag.source

	property var containsItem: null

	onContainsItemChanged: {
		//console.log(__debugName," onContainsItemChanged to " + (containsItem !== null ? containsItem.__debugName : "null"))

		if(containsItem === null)
			width = Qt.binding(function(){ return dropText.contentWidth })
		iWasChecked = false

	}

	Item
	{
		id: dropText

		property string text: dragTarget.defaultText
		property real contentWidth: dropTextStatic.visible ? dropTextStatic.contentWidth : dropTextInput.contentWidth

		anchors.top: parent.top
		anchors.bottom: parent.bottom
		anchors.horizontalCenter: parent.horizontalCenter

		width: dropTextStatic.visible ? dropTextStatic.width : dropTextInput.width
		//height: dropTextStatic.visible ? dropTextStatic.height : dropTextInput.height

		states: [
			State {	when:  dragTarget.containsDrag;	PropertyChanges { target: dropText; text: "";						} },
			State {	when: !dragTarget.containsDrag;	PropertyChanges { target: dropText; text: dragTarget.defaultText;	} }
		]

		Text
		{
			id:				dropTextStatic

			text:			dragTarget.containsItem === null ? dropText.text : ""
			font.pixelSize: filterConstructor.fontPixelSize
			anchors.top:	parent.top

			visible:		!dropTextInput.visible
			color:			jaspTheme.textEnabled
		}

		TextInput
		{
			id:				dropTextInput

			text:			dropText.text
			color:			errorMarker.visible ? jaspTheme.white : jaspTheme.black
			font.pixelSize: filterConstructor.fontPixelSize
			anchors.top:	parent.top

			visible:		dragTarget.acceptsDrops && dragTarget.containsItem === null

			onAccepted:		focus = false

			onActiveFocusChanged: {
				//console.log(__debugName + " onActiveFocusChanged to " + (activeFocus? "true" : "false"))
				if(!activeFocus)
					tryConvertToObject()

				//console.log("dragTarget.containsItem: " + (dragTarget.containsItem === null ? "null" : dragTarget.containsItem.__debugName))

				var keepItEmpty = activeFocus || dragTarget.containsItem
				dropText.text = keepItEmpty ? "" : dragTarget.defaultText
				//console.log("Setting text to " +  (keepItEmpty ? "" : dragTarget.defaultText));

				if(!activeFocus) text = Qt.binding(function(){return dropText.text})
			}

			function tryConvertToObject()
			{
				//console.log("tryConvertToObject")
				if(dragTarget.containsItem !== null)
					return

				var asNumber = parseFloat(text)
				if(!isNaN(asNumber) && dropKeys.indexOf("number") >= 0)
					createNumber(asNumber)
				else if(dropKeys.indexOf("string") >= 0 && text !== "")
					createString(text)
			}

			function createNumber(value)	{ setCreatedObjectUp(numberComp.createObject(dragTarget, { "value": value,  "canBeDragged": true, "acceptsDrops": true } ) ) }
			function createString(string)	{ setCreatedObjectUp(stringComp.createObject(dragTarget, { "text":  string, "canBeDragged": true, "acceptsDrops": true } ) ) }


			function setCreatedObjectUp(obj)
			{
				//console.log("setCreatedObjectUp");
				filterConstructor.somethingChanged = true
				dragTarget.originalWidth = dragTarget.width
				dragTarget.width = Qt.binding(function(){ return obj.width } )

				obj.releaseHere(dragTarget)
			}
		}

		Rectangle
		{
			id: errorMarker
			z: -2
			visible: (dragTarget.iWasChecked && dragTarget.containsItem === null)
			radius: width
			anchors.fill: parent
			color: "#BB0000"
		}
	}

	Component { id: numberComp; NumberDrag {}}
	Component { id: stringComp; StringDrag {}}


}
