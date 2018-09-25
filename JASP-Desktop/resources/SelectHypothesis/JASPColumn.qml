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
    id: jaspColumnRoot
    objectName: "Column"
    property string __debugName: "JASPColumn " + columnName
    property string columnName: "?"

    property real maxSize: 200
    property real colScaler: 0.8
    height: filterConstructor.blockDim * colScaler
    width: colName.width

    property var dragKeys: ["string"]

    TextMetrics {
        id: columnNameMeasure
        text: columnName
    }

    Text {
        id: colName
        anchors.top: parent.top
        anchors.left: parent.left
        anchors.bottom: parent.bottom
        width: Math.min(columnNameMeasure.width + 10, jaspColumnRoot.maxSize)

        font.pixelSize: filterConstructor.fontPixelSize * colScaler

        leftPadding: 2

        text: columnName
        elide: Text.ElideMiddle
    }

    function shouldDrag(mouseX, mouseY) {
        return true
    }
    function returnEmptyRightMostDropSpot() {
        return null
    }
    function returnFilledRightMostDropSpot() {
        return null
    }
    function returnR() {
        return columnName
    }
    function checkCompletenessFormulas() {
        return true
    }
    function convertToJSON() {
        var jsonObj = {
            nodeType: "Column",
            columnName: columnName
        }
        return jsonObj
    }
}
