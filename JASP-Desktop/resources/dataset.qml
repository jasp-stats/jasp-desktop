import QtQuick.Controls 2.1
import QtQuick.Controls.Styles 1.4
import QtQuick.Controls 1.4
import QtQuick 2.7


Rectangle {
    SystemPalette { id: systemPalette; colorGroup: SystemPalette.Active }
    id: rootDataset
    color: systemPalette.window


    Rectangle {
        id: variablesWindow
        color: "blue"
        height: 0
        anchors.left: rootDataset.left
        anchors.right: rootDataset.right

        property bool opened: false

        states: [
            State {
                name: "closed"
                PropertyChanges { target: variablesWindow; height: 0 }
                when: !variablesWindow.opened
            },
            State {
                name: "opened"
                PropertyChanges { target: variablesWindow; height: 200 }
                when: variablesWindow.opened
            }
        ]

        transitions: Transition { NumberAnimation { properties: "height"; easing.type: Easing.InOutQuad } }
    }

    TableViewJasp {
        id: dataSetTableView
        objectName: "dataSetTableView"
        anchors.top: variablesWindow.bottom
        anchors.left: rootDataset.left
        anchors.right: rootDataset.right
        anchors.bottom: rootDataset.bottom

        alternatingRowColors: true
        property bool drawCellBorders: true

        model: dataSetModel

        function reloadColumns()
        {
            var roleList = dataSetModel.userRoleNames();

            //data.clear()
            for(var i=0; i<roleList.length; i++)
                data.push(dataSetTableView.addColumn(columnComponent.createObject(dataSetTableView, { "role": roleList[i], "title": dataSetModel.columnTitle(i)})))

            resizeColumnsWithHeader()
        }


        function resizeColumnsWithHeader()
        {
            var extraPaddingRight = 15
            for(var col=0; col<columnCount; col++)
            {
                var column = getColumn(col)
                var title = dataSetModel.columnTitle(col)
                var tempCalc = columnHeaderSizeCalcComponent.createObject(dataSetTableView, { "text": title})
                var minimumWidth = tempCalc.width + 10 + tempCalc.height + extraPaddingRight

                for(var row=0; row<rowCount; row++)
                {
                    var rowVal = dataSetModel.getCellValue(col, row)
                    tempCalc.text = rowVal
                    minimumWidth = Math.max(minimumWidth, tempCalc.width + extraPaddingRight)
                }

                //Lets resize content while keeping it larger than the headerText + some iconSize
                column.width = minimumWidth
            }
        }

        Component { id: columnComponent; TableViewColumn { movable: false } }
        Component { id: columnHeaderSizeCalcComponent; TextMetrics { } }

        headerDelegate: Rectangle
        {
            //Two rectangles to show a border of exactly 1px around cells
            id: headerBorderRectangle
            color: systemPalette.mid
            border.width: 0
            radius: 0

            property real iconDim: headerText.implicitHeight
            property real iconTextPadding: 10
            height: iconDim * 1.2

            z: styleData.column

            //implicitWidth: headerText.contentWidth + iconDim + 5 + iconTextPadding

            Rectangle
            {
                id: colHeader
                gradient: Gradient{
                    GradientStop { position: 0.6; color: systemPalette.light }
                    GradientStop { position: 0.7; color: systemPalette.midlight }
                }

                x: headerBorderRectangle.x
                y: headerBorderRectangle.y + 1

                height: headerBorderRectangle.height - 2
                width: headerBorderRectangle.width - 1

                Image
                {
                    id: colIcon
                    anchors.top: colHeader.top
                    anchors.bottom: colHeader.bottom
                    anchors.left: colHeader.left
                    source: dataSetModel.columnIcon(styleData.column)

                    anchors.leftMargin: 4

                    width: headerBorderRectangle.iconDim
                    height: headerBorderRectangle.iconDim


                    MouseArea
                    {
                        anchors.fill: parent

                        onPressed:
                        {
                            popup.open()
                        }
                    }


                    Popup {
                        id: popup
                        x: parent.x
                        y: parent.y + parent.height
                        width: 100
                        height: 100
                        modal: true
                        focus: true
                        closePolicy: Popup.CloseOnReleaseOutside | Popup.CloseOnPressOutside | Popup.CloseOnEscape
                    }
                }


                Text
                {
                    id: headerText
                    anchors.top: colHeader.top
                    anchors.bottom: colHeader.bottom
                    anchors.left: colIcon.right
                    anchors.right: colHeader.right
                    text: styleData.value //dataSetModel.columnTitle(styleData.column);
                    leftPadding: iconTextPadding

                    MouseArea
                    {
                        anchors.fill: parent
                        onClicked:
                        {
                            console.log("HeaderText pressed!")
                            variablesWindow.opened = !variablesWindow.opened
                        }
                    }
                }
            }
        }

        itemDelegate: Item
        {
            Rectangle
            {
                //Two rectangles to show a border of exactly 1px around cells
                id: borderRectangle
                color: "transparent"
                border.width: dataSetTableView.drawCellBorders ? 1 : 0
                border.color: systemPalette.mid
                radius: 0
                width: parent.width + 1
                height: parent.height + 1
                x: parent.x - 1
                y: parent.y - 1

                Text {
                    id: itemText
                    text: styleData.value
                    color: systemPalette.text
                    elide: styleData.elideMode
                    horizontalAlignment: styleData.textAlignment
                    leftPadding: 4
                }
            }

        }
    }
}
