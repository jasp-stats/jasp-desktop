import QtQuick 2.9
import QtQuick.Controls 2.2


MouseArea
{
	//Rectangle { color: "transparent"; border.color: "purple"; border.width: 1; anchors.fill: parent } //debug for size of the drags

	id: mouseArea

	z: 2

	property var alternativeDropFunction: null

	property real dragHotSpotX: width / 2
	property real dragHotSpotY: height / 2

	property real dragX: dragMe.x - mapToItem(dragMe.parent, 0, 0).x
	property real dragY: dragMe.y - mapToItem(dragMe.parent, 0, 0).y

	property var leftDropSpot: null //should only contain something for operators I guess?

	readonly property bool isFormula: parent !== undefined && parent !== null && parent.objectName === "scriptColumn"
	readonly property bool isABoolean: dragKeys.indexOf("boolean") >= 0
	property bool wasChecked: false

	property string __debugName: "DragGeneric " + shownChild !== undefined ? shownChild.__debugName : "?"

	property string toolTipText: ""
	property string shownToolTipText: toolTipText

	ToolTip.delay: 1000
	ToolTip.timeout: 5000
	ToolTip.visible: shownToolTipText != "" && containsMouse
	ToolTip.text: shownToolTipText

	objectName: "DragGeneric"
	property var shownChild: null

	property alias dragChild: dragMe

	width: shownChild == null ? implicitWidth : shownChild.width
	height: shownChild == null ? implicitHeight : shownChild.height

	property bool nested: parent !== null && parent.objectName === "DropSpot" && parent.droppedShouldBeNested

	property var dragKeys: [ "number", "boolean", "string", "variable" ] //all possible options by default

	drag.target: dragMe

	property var oldParent: null

	hoverEnabled: true
	cursorShape: (containsMouse && shownChild.shouldDrag(mouseX, mouseY)) || drag.active  ? Qt.PointingHandCursor : Qt.ArrowCursor
	acceptedButtons: Qt.LeftButton | Qt.RightButton

	property bool shouldShowHoverOutline:	false
	property bool showHighlight:			shownChild !== null ? !shownChild.acceptsDrops : false

	Rectangle
	{
		color:				jaspTheme.blueMuchLighter
		visible:			parent.showHighlight && parent.containsMouse
		z:					-6
		anchors.centerIn:	parent
		radius:				4 * preferencesModel.uiScale
		height:				parent.height + radius
		width:				parent.width  + radius
	}

	onPositionChanged:
	{
		if(!mouseArea.drag.active)
			shouldShowHoverOutline = shownChild.shouldDrag(mouseX, mouseY)

	}

	onExited:
	{
		//console.log(__debugName," onExited")
		shouldShowHoverOutline = false
	}

	onEntered:
	{
		//console.log(__debugName," onEntered")
		this.removeAncestorsHoverOutlines()
	}

	function removeAncestorsHoverOutlines()
	{
		//console.log(__debugName," removeAncestorsHoverOutlines")
		var ancestor = parent
		while(ancestor !== scriptColumn && ancestor !== null && ancestor !== undefined)
		{
			if(ancestor.objectName === "DragGeneric")
				ancestor.shouldShowHoverOutline = false
			ancestor = ancestor.parent
		}
	}


	onPressed:
	{
		//console.log(__debugName," onPressed")

		if(mouse.buttons === Qt.RightButton)
		{
			//delete me!
			if(alternativeDropFunction === null)
				this.destroy();
		}
		else
		{

			shouldShowHoverOutline = false
			oldParent = parent

			if(!shownChild.shouldDrag(mouse.x, mouse.y))
				mouse.accepted = false
			else
			{
				mouseArea.dragHotSpotX = mouse.x
				mouseArea.dragHotSpotY = mouse.y
			}
		}
	}

	onReleased:
	{
		if(mouseArea.alternativeDropFunction !== null)
		{
			var obj = mouseArea.alternativeDropFunction(mouseArea)
			if(obj !== null)
				obj.releaseHere(dragMe.Drag.target)
		}
		else
			mouseArea.releaseHere(dragMe.Drag.target)
	}


	function releaseHere(dropTarget)
	{
		//console.log(__debugName," releaseHere(",dropTarget,")")

		filterConstructor.somethingChanged = true
		wasChecked = false

		if(oldParent === null && dropTarget === null)
		{
			//console.log("just created and not dropped anywhere specific!\nSo lets try to find a better place, making it as userfriendly as possible")

			var newDropTarget = this.determineReasonableInsertionSpot();

			if(newDropTarget !== null)
			{
				//console.log("Found a new droptarget: " + newDropTarget.__debugName)
				this.releaseHere(newDropTarget)
				return
			}

            if(leftDropSpot !== null && this.tryLeftApplication()) //maybe gobble something up instead of the other way 'round?
               return
		}

		//console.log("Second half of release here")

		if(dropTarget !== null && dropTarget.objectName === "DropSpot")
		{
			//console.log("it is in fact dropped on a dropspot!");

			var foundAtLeastOneMatchingKey = false
			for(var dragI=0; dragI<dragKeys.length; dragI++)
				if(dropTarget.dropKeys.indexOf(dragKeys[dragI]) >= 0)
					foundAtLeastOneMatchingKey = true

			if(!foundAtLeastOneMatchingKey)
			{
				//console.log("Didnt find a matching key...")
				this.releaseHere(scriptColumn)
				return
			}
		}


		if(oldParent !== null && oldParent.objectName === "DropSpot" && dropTarget !== oldParent && oldParent.containsItem === this)
		{
			//console.log("restoring size of oldParent: " + oldParent.__debugName)
			oldParent.width			= oldParent.implicitWidth
			oldParent.height		= oldParent.implicitHeight
			oldParent.containsItem	= null
		}

		if(dropTarget !== null && dropTarget.objectName === "DropTrash")
		{
			//console.log("Dropped in trash!")
			this.destroy();
			return;
		}

		parent = dropTarget !== null ? dropTarget : scriptColumn

		//console.log("Set parent to " + parent.__debugName)

		if(parent === oldParent )
		{
			parent = null
			parent = oldParent
		}



		dragMe.x = 0
		dragMe.y = 0
		mouseArea.x = 0
		mouseArea.y = 0

		if(parent.objectName === "DropSpot")
		{
			//console.log("Setting " + this.__debugName + " to containsItem in parent " + parent.__debugName)
			parent.width  = Qt.binding(function() { return dragMe.width })
			parent.height = Qt.binding(function() { return dragMe.height })
			parent.containsItem = this

			shouldShowHoverOutline = false
			this.removeAncestorsHoverOutlines()
		}

		scriptColumn.focus = true
	}

	function determineReasonableInsertionSpot()
	{
		//console.log(__debugName," determineReasonableInsertionSpot")

		if(scriptColumn.data.length === 0) return null

		var lastScriptScrap = scriptColumn.data[scriptColumn.data.length - 1]

		if(lastScriptScrap === this)
		{
			if(scriptColumn.data.length === 1)
				return null

			lastScriptScrap = scriptColumn.data[scriptColumn.data.length - 2]
			if(lastScriptScrap === this)
			{
				console.log("Somehow the function to determineReasonableInsertionSpot found itself, which should be impossible.")
				return null //cannot happen hopefully?
			}
		}

		return lastScriptScrap.returnEmptyRightMostDropSpot(true)
	}

	//onParentChanged: { console.log(__debugName," onParentChanged parent == ", parent === null ? "null" : parent === undefined ? "undefined" : parent.__debugName, " alternativeDropFunction == ", alternativeDropFunction === null ? "null" : alternativeDropFunction === undefined ? "undefined" : alternativeDropFunction)	}

	function returnR()							{ return shownChild.returnR() }
	function returnEmptyRightMostDropSpot()		{ return shownChild.returnEmptyRightMostDropSpot() }
	function returnFilledRightMostDropSpot()	{ return shownChild.returnFilledRightMostDropSpot() }
	function checkCompletenessFormulas()		{ wasChecked = true; return shownChild.checkCompletenessFormulas() }
	function convertToJSON()					{ var obj = shownChild.convertToJSON(); obj.toolTipText = toolTipText; return obj }

	function tryLeftApplication()
	{
		//console.log(__debugName," tryLeftApplication")

		this.releaseHere(scriptColumn)

        if(leftDropSpot === null || leftDropSpot.containsItem !== null || scriptColumn.data.length === 1) return false

		for(var i=scriptColumn.data.length - 1; i>=0; i--)
			if(scriptColumn.data[i] !== this)
			{
				var gobbleMeUp = scriptColumn.data[i]
				var putResultHere = scriptColumn

				while(gobbleMeUp !== null && putResultHere !== null && gobbleMeUp !== undefined)
				{

					for(var keyI=0; keyI<gobbleMeUp.dragKeys.length; keyI++)
						if(leftDropSpot.dropKeys.indexOf(gobbleMeUp.dragKeys[keyI])>=0)
							for(var myDragKeyI=0; myDragKeyI<this.dragKeys.length; myDragKeyI++)
								if(putResultHere === scriptColumn || putResultHere.dropKeys.indexOf(dragKeys[myDragKeyI]) >= 0) //Make sure we are allowed to drop ourselves there!
								{
									gobbleMeUp.releaseHere(scriptColumn)

									if(putResultHere !== scriptColumn) //we went deeper
										this.releaseHere(putResultHere)

									gobbleMeUp.releaseHere(leftDropSpot)

									return true
								}


					//Ok, we couldnt actually take the entire node into ourselves. Maybe only the right part?
					//Which means we have to place ourselves in the dropSpot under the current gobbleMeUp!

					putResultHere	= gobbleMeUp.returnFilledRightMostDropSpot()
					if(putResultHere === null) return
					gobbleMeUp		= putResultHere.containsItem

					//if(gobbleMeUp.objectName !== "DragGeneric")
					//	console.log("gobbleMeUp: ", gobbleMeUp, " is not a dragGeneric but a ",	gobbleMeUp.objectName)

				}

                return false
			}
	}

	Item
	{
		id: dragMe

		width: mouseArea.width
		height: mouseArea.height
		x: mouseArea.x
		y: mouseArea.y

		Drag.keys: ["all"]
		Drag.active: mouseArea.drag.active
		Drag.hotSpot.x: mouseArea.dragHotSpotX
		Drag.hotSpot.y: mouseArea.dragHotSpotY

		property alias dragKeys: mouseArea.dragKeys

		Rectangle
		{
			id:	dragHandleVisualizer
			color: "transparent"
			radius: width
			property real maxWidth: 12
			height: width
			border.color: "#14a1e3"
			border.width: 2

			x: mouseArea.dragHotSpotX - (width / 2)
			y: mouseArea.dragHotSpotY - (height / 2)
			z: 10

			visible: mouseArea.drag.active
			SequentialAnimation on width {
				NumberAnimation { from: 1; to: dragHandleVisualizer.maxWidth; duration: 500;  }
				NumberAnimation { from: dragHandleVisualizer.maxWidth; to: 1; duration: 500;  }
				loops: Animation.Infinite
				paused: !dragHandleVisualizer.visible
			}

		}

		states: [
			State {
				when: mouseArea.drag.active
				ParentChange	{ target: dragMe; parent: filterConstructor }
				AnchorChanges	{ target: dragMe; anchors.verticalCenter: undefined; anchors.horizontalCenter: undefined }
			},
			State {
				when: !mouseArea.drag.active
				AnchorChanges	{ target: dragMe; anchors.verticalCenter: mouseArea.verticalCenter; anchors.horizontalCenter: mouseArea.horizontalCenter }
			}
		]

	}
}



