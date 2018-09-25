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


Item {
    id: opRoot
    objectName: "Operator"
    property string __debugName: "Operator " + operator

    property int initialWidth: filterConstructor.blockDim * acceptsDrops ? 4 : 2
    property string operator: "+"
    property string operatorImageSource: ""
    property bool acceptsDrops: true
    property bool isNested: false
    property var leftDropSpot: leftDrop
    property var dropKeys: ["number"]
    property bool dropKeysMirrorEachother: false
    property var dropKeysLeft: dropKeys
    property var dropKeysRight: dropKeys

    property alias leftDrop: leftDrop
    property alias rightDrop: rightDrop

    height: Math.max(filterConstructor.blockDim, leftDrop.height,
                     rightDrop.height)
    width: opX + opWidth + rightDrop.width + (haakjesRechts.visible ? haakjesRechts.width : 0)
    property real opWidth: opImg.visible ? opImg.width + 2 : opText.width
    property real opX: opImg.visible ? opImg.x : opText.x

    function shouldDrag(mouseX, mouseY) {
        if (!acceptsDrops)
            return true

        return mouseX <= haakjesLinks.width || mouseX > haakjesRechts.x
                || (mouseX > opX && mouseX < opX + opWidth)
    }

    function returnR() {
        var compounded = "" //"("
        compounded += leftDrop.containsItem !== null ? leftDrop.containsItem.returnR(
                                                           ) : "null"
        compounded += " " + operator + " "
        compounded += rightDrop.containsItem !== null ? rightDrop.containsItem.returnR(
                                                            ) : "null"

        // compounded += ")"
        return compounded
    }

    function returnEmptyRightMostDropSpot() {
        if (rightDrop.containsItem !== null)
            return rightDrop.containsItem.returnEmptyRightMostDropSpot()
        return rightDrop
    }

    function returnFilledRightMostDropSpot() {

        if (rightDrop.containsItem !== null)
            return rightDrop
        return null
    }

    function checkCompletenessFormulas() {

        var leftIsOk = leftDrop.checkCompletenessFormulas()
        var rightIsOk = rightDrop.checkCompletenessFormulas()
        return leftIsOk && rightIsOk
    }

    function convertToJSON() {
        var jsonObj = {
            nodeType: "Operator",
            operator: operator,
            leftArgument: (leftDrop.containsItem
                           === null ? null : leftDrop.containsItem.convertToJSON(
                                          )),
            rightArgument: (rightDrop.containsItem
                            === null ? null : rightDrop.containsItem.convertToJSON(
                                           ))
        }
        return jsonObj
    }

    Text {
        id: haakjesLinks
        anchors.top: parent.top
        anchors.bottom: parent.bottom

        verticalAlignment: Text.AlignVCenter
        horizontalAlignment: Text.AlignHCenter

        text: opRoot.isNested ? "(" : ""
        font.pixelSize: filterConstructor.fontPixelSize

        visible: opRoot.isNested
    }

    DropSpot {

        dropKeys: !(opRoot.dropKeysMirrorEachother && rightDrop.containsItem
                    !== null) ? opRoot.dropKeysLeft : rightDrop.containsItem.dragKeys
        id: leftDrop

        x: haakjesLinks.width
        anchors.verticalCenter: parent.verticalCenter

        acceptsDrops: parent.acceptsDrops
        droppedShouldBeNested: false // true
    }

    Image {
        id: opImg
        x: leftDrop.x + leftDrop.width + 2

        visible: operatorImageSource !== ""

        source: operatorImageSource
        sourceSize.width: filterConstructor.blockDim * 2
        sourceSize.height: filterConstructor.blockDim * 2

        height: filterConstructor.blockDim
        width: height
        anchors.verticalCenter: parent.verticalCenter
    }

    Text {
        id: opText
        anchors.top: parent.top
        anchors.bottom: parent.bottom

        leftPadding: 2
        rightPadding: 2
        x: leftDrop.x + leftDrop.width

        verticalAlignment: Text.AlignVCenter
        horizontalAlignment: Text.AlignHCenter

        text: opRoot.operator
        font.pixelSize: filterConstructor.fontPixelSize

        visible: !opImg.visible

        font.bold: true
    }

    DropSpot {
        dropKeys: !(opRoot.dropKeysMirrorEachother && leftDrop.containsItem
                    !== null) ? opRoot.dropKeysRight : leftDrop.containsItem.dragKeys
        anchors.verticalCenter: parent.verticalCenter

        id: rightDrop
        x: opX + opWidth

        acceptsDrops: parent.acceptsDrops
        droppedShouldBeNested: false // true
    }

    Text {

        id: haakjesRechts

        anchors.top: parent.top
        anchors.bottom: parent.bottom
        x: rightDrop.x + rightDrop.width

        verticalAlignment: Text.AlignVCenter
        horizontalAlignment: Text.AlignHCenter

        text: opRoot.isNested ? ")" : ""
        font.pixelSize: filterConstructor.fontPixelSize
        visible: opRoot.isNested
    }
}
