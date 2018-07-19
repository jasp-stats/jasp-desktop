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
