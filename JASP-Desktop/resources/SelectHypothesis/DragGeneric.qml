//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

import QtQuick 2.9
import QtQuick.Controls 2.2


MouseArea {
    id: mouseArea

    z: 2

    property var alternativeDropFunction: null
    property real dragHotSpotX: width / 2
    property real dragHotSpotY: height / 2

    property real dragX: dragMe.x - mapToItem(dragMe.parent, 0, 0).x
    property real dragY: dragMe.y - mapToItem(dragMe.parent, 0, 0).y

    property var leftDropSpot: null

    readonly property bool isFormula: parent !== undefined && parent !== null
                                      && parent.objectName === "scriptColumn"
    readonly property bool isABoolean: dragKeys.indexOf("boolean") >= 0
    property bool wasChecked: false

    property string __debugName: "DragGeneric " + shownChild
                                 !== undefined ? shownChild.__debugName : "?"

    Rectangle {
        id: formulaNonBoolean

        color: "transparent"
        border.color: visibleBecauseOfMouseHover ? "#70c0ff" : "red"
        border.width: visibleBecauseOfMouseHover ? 2 : 1
        radius: visibleBecauseOfMouseHover ? 10 : 0
        anchors.margins: visibleBecauseOfMouseHover ? -3 : 0

        anchors.fill: parent

        property bool visibleBecauseofCheckedError: mouseArea.wasChecked
                                                    && mouseArea.isFormula
                                                    && !mouseArea.isABoolean
        property alias visibleBecauseOfMouseHover: mouseArea.shouldShowHoverOutline

        visible: visibleBecauseofCheckedError || visibleBecauseOfMouseHover

        z: -2
    }

    property string toolTipText: ""
    property string shownToolTipText: formulaNonBoolean.visible ? "This formula should return logicals, try using '=', '>' or something similar." : toolTipText

    ToolTip.delay: 1000
    ToolTip.timeout: 5000
    ToolTip.visible: shownToolTipText != "" && containsMouse
    ToolTip.text: shownToolTipText

    objectName: "DragGeneric"
    property var shownChild: null

    property alias dragChild: dragMe

    width: shownChild == null ? implicitWidth : shownChild.width
    height: shownChild == null ? implicitHeight : shownChild.height

    property bool nested: parent !== null && parent.objectName === "DropSpot"
                          && parent.droppedShouldBeNested

    property var dragKeys: ["number", "boolean", "string", "variable"] //all possible options by default

    drag.target: dragMe

    property var oldParent: null

    hoverEnabled: true
    cursorShape: (containsMouse && shownChild.shouldDrag(mouseX, mouseY))
                 || drag.active ? Qt.PointingHandCursor : Qt.ArrowCursor
    acceptedButtons: Qt.LeftButton | Qt.RightButton

    property bool shouldShowHoverOutline: false

    onPositionChanged: {
        if (!mouseArea.drag.active)
            shouldShowHoverOutline = shownChild.shouldDrag(mouseX, mouseY)
    }

    onExited: {
        shouldShowHoverOutline = false
    }

    onEntered: {
        this.removeAncestorsHoverOutlines()
    }

    function removeAncestorsHoverOutlines() {
        var ancestor = parent

        while (ancestor !== scriptColumn && ancestor !== null
               && ancestor !== undefined) {
            if (ancestor.objectName === "DragGeneric")
                ancestor.shouldShowHoverOutline = false
            ancestor = ancestor.parent
        }
    }

    onPressed: {

        if (mouse.buttons === Qt.RightButton) {
            // delete me
            if (alternativeDropFunction === null)
                this.destroy()
        } else {

            shouldShowHoverOutline = false
            oldParent = parent

            if (!shownChild.shouldDrag(mouse.x, mouse.y)) {
                mouse.accepted = false
            } else {
                mouseArea.dragHotSpotX = mouse.x
                mouseArea.dragHotSpotY = mouse.y
            }
        }
    }

    onReleased: {

        if (alternativeDropFunction !== null) {
            var obj = this.alternativeDropFunction(this)
            if (obj !== null)
                obj.releaseHere(dragMe.Drag.target)
        } else {
            this.releaseHere(dragMe.Drag.target)
        }
    }

    function releaseHere(dropTarget) {

        filterConstructor.somethingChanged = true
        wasChecked = false

        if (oldParent === null && dropTarget === null) {

            var newDropTarget = this.determineReasonableInsertionSpot(
                        ) // So lets try to find a better place, make it as userfriendly as possible

            if (newDropTarget !== null) {
                this.releaseHere(newDropTarget)
                return
            }

            if (leftDropSpot !== null && this.tryLeftApplication())
                // maybe gobble something up instead of the other way 'round?
                return
        }

        if (dropTarget !== null && dropTarget.objectName === "DropSpot") {

            var foundAtLeastOneMatchingKey = false

            for (var dragI = 0; dragI < dragKeys.length; dragI++)
                if (dropTarget.dropKeys.indexOf(dragKeys[dragI]) >= 0)
                    foundAtLeastOneMatchingKey = true

            if (!foundAtLeastOneMatchingKey) {
                this.releaseHere(scriptColumn)
                return
            }
        }

        if (oldParent !== null && oldParent.objectName === "DropSpot"
                && dropTarget !== oldParent) {
            oldParent.width = oldParent.implicitWidth
            oldParent.height = oldParent.implicitHeight
            oldParent.containsItem = null
        }

        if (dropTarget !== null && dropTarget.objectName === "DropTrash") {
            this.destroy()
            dropTarget.somethingHovers = false
            return
        }

        parent = dropTarget !== null ? dropTarget : scriptColumn

        if (parent === oldParent) {
            parent = null
            parent = oldParent
        }

        dragMe.x = 0
        dragMe.y = 0
        mouseArea.x = 0
        mouseArea.y = 0

        if (parent.objectName === "DropSpot") {
            parent.width = Qt.binding(function () {
                return dragMe.width
            })
            parent.height = Qt.binding(function () {
                return dragMe.height
            })
            parent.containsItem = this

            shouldShowHoverOutline = false
            this.removeAncestorsHoverOutlines()
        }

        scriptColumn.focus = true
    }

    function determineReasonableInsertionSpot() {

        if (scriptColumn.data.length === 0)
            return null

        var lastScriptScrap = scriptColumn.data[scriptColumn.data.length - 1]

        if (lastScriptScrap === this) {
            if (scriptColumn.data.length === 1)
                return null

            lastScriptScrap = scriptColumn.data[scriptColumn.data.length - 2]
            if (lastScriptScrap === this)
                return null //cannot happen hopefully?
        }

        return lastScriptScrap.returnEmptyRightMostDropSpot(true)
    }

    function returnR() {
        return shownChild.returnR()
    }
    function returnEmptyRightMostDropSpot() {
        return shownChild.returnEmptyRightMostDropSpot()
    }
    function returnFilledRightMostDropSpot() {
        return shownChild.returnFilledRightMostDropSpot()
    }
    function checkCompletenessFormulas() {
        wasChecked = true
        return shownChild.checkCompletenessFormulas()
    }
    function convertToJSON() {
        var obj = shownChild.convertToJSON()
        obj.toolTipText = toolTipText
        return obj
    }

    function tryLeftApplication() {

        this.releaseHere(scriptColumn)

        if (leftDropSpot === null || leftDropSpot.containsItem !== null
                || scriptColumn.data.length === 1)
            return false

        for (var i = scriptColumn.data.length - 1; i >= 0; i--)
            if (scriptColumn.data[i] !== this) {
                var gobbleMeUp = scriptColumn.data[i]
                var putResultHere = scriptColumn

                while (gobbleMeUp !== null && putResultHere !== null
                       && gobbleMeUp !== undefined) {

                    for (var keyI = 0; keyI < gobbleMeUp.dragKeys.length; keyI++)
                        if (leftDropSpot.dropKeys.indexOf(
                                    gobbleMeUp.dragKeys[keyI]) >= 0)
                            for (var myDragKeyI = 0; myDragKeyI
                                 < this.dragKeys.length; myDragKeyI++)
                                if (putResultHere === scriptColumn
                                        || putResultHere.dropKeys.indexOf(
                                            dragKeys[myDragKeyI]) >= 0) {
                                    //Make sure we are allowed to drop ourselves there!
                                    gobbleMeUp.releaseHere(scriptColumn)

                                    if (putResultHere !== scriptColumn)
                                        //we went deeper
                                        this.releaseHere(putResultHere)

                                    gobbleMeUp.releaseHere(leftDropSpot)
                                    return true
                                }


                    //Ok, we couldnt actually take the entire node into ourselves. Maybe only the right part?
                    //Which means we have to place ourselves in the dropSpot under the current gobbleMeUp!
                    putResultHere = gobbleMeUp.returnFilledRightMostDropSpot()
                    if (putResultHere === null)
                        return
                    gobbleMeUp = putResultHere.containsItem
                }

                return false
            }
    }

    Item {
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

        Rectangle {
            id: dragHandleVisualizer
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
                NumberAnimation {
                    from: 1
                    to: dragHandleVisualizer.maxWidth
                    duration: 500
                }
                NumberAnimation {
                    from: dragHandleVisualizer.maxWidth
                    to: 1
                    duration: 500
                }
                loops: Animation.Infinite
                paused: !dragHandleVisualizer.visible
            }
        }

        states: [
            State {
                when: mouseArea.drag.active
                ParentChange {
                    target: dragMe
                    parent: filterConstructor
                }
                AnchorChanges {
                    target: dragMe
                    anchors.verticalCenter: undefined
                    anchors.horizontalCenter: undefined
                }
            },
            State {
                when: !mouseArea.drag.active
                AnchorChanges {
                    target: dragMe
                    anchors.verticalCenter: mouseArea.verticalCenter
                    anchors.horizontalCenter: mouseArea.horizontalCenter
                }
            }
        ]
    }
}
