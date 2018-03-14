import QtQuick 2.9
import QtQuick.Controls 2.3
import QtQuick.Controls 1.4
import QtGraphicalEffects 1.0

FocusScope
{
    id: __myRoot
    readonly property int __iconDim: 16
    readonly property int __headerHeight: 20

    property var headersGradient: Gradient{
        GradientStop { position: 0.2; color: systemPalette.midlight }
        GradientStop { position: 1.0; color: systemPalette.light }
    }

    Item
    {
        id: columnTypes
        //Copied from column.h, should actually be exported or something like that
        readonly property int columnTypeScale: 0
        readonly property int columnTypeOrdinal: 1
        readonly property int columnTypeNominal: 2
        readonly property int columnTypeNominalText: 3
    }


    Rectangle
    {
        id: filterToggleButton

        width: dataSetTableView.extraSpaceLeft + 1
        height: __myRoot.__headerHeight + 1
        anchors.left: parent.left
        anchors.top: parent.top
        z: 5

        LinearGradient
        {

            cached: true
            anchors.fill: parent
            start: Qt.point(parent.width * 0.5, parent.height * 0.5)
            end: Qt.point(parent.width, parent.height)
            gradient: headersGradient
        }

        Rectangle
        {
            anchors.fill: parent
            border.width: 1
            border.color: systemPalette.mid
            color: "transparent"
            z: 6
        }

        Image
        {
            source: "../images/filter.png"
            width: __myRoot.__iconDim
            height: __myRoot.__iconDim
            x: parent.x + (parent.width / 2) - (width / 2)
            y: parent.y + (parent.height / 2) - (height / 2)
        }

        MouseArea
        {
            id: filterToggleButtonMouseArea
            anchors.fill: parent
            onClicked: filterWindow.toggle()
            hoverEnabled: true
            onEntered: filterToggleButtonToolTip.visible = true
            onExited: filterToggleButtonToolTip.visible = false
        }

        ToolTip
        {
             id: filterToggleButtonToolTip
             text: "Toggle visibilty of filtering functionality"
            // target: filterToggleButton
             visible: false
         }

    }

    TableViewJasp {
        id: dataSetTableView
        objectName: "dataSetTableView"

        anchors.top: parent.top
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.bottom: dataStatusBar.top

        alternatingRowColors: false
        property bool drawCellBorders: true


        model: dataSetModel

        readonly property var columnTypeAndIconPath: dataSetModel.getColumnTypesWithCorrespondingIcon(true)
        readonly property var columnTypeChangeIconPaths:dataSetModel.getColumnTypesWithCorrespondingIcon(false)

        function clearColumns()
        {
            dataSetTableView.removeAllColumns()
        }
        
        function reloadColumns()
        {
            clearColumns();

            var roleList = dataSetModel.userRoleNames();

            //data.clear()
            for(var i=0; i<roleList.length; i++)
                data.push(dataSetTableView.addColumn(columnComponent.createObject(dataSetTableView, { "role": roleList[i], "title": dataSetModel.columnTitle(i)}))) //should use headerData instead of columnTitle.

            resizeColumnsWithHeader()
        }

        Component { id: columnComponent; TableViewColumn { movable: false } }


        function resizeColumnsWithHeader()
        {
            var extraPadding = 15
            for(var col=0; col<columnCount; col++)
            {
                var title = dataSetModel.columnTitle(col)
                var minimumWidth = calculateMinimumRequiredColumnWidthTitle(col, title, 20 + extraPadding, 1000)
                var column = getColumn(col)
                column.width = minimumWidth
            }

            //Try to find a reasonable size for our ugly rownumberthing:
            var modelCount = dataSetModel.rowCount()
            var tempCalc = rowNumberSizeCalcComponent.createObject(dataSetTableView, { "text": modelCount})
            var extraSpaceShouldBe = tempCalc.width + 4
            tempCalc.destroy()
            extraSpaceLeft = extraSpaceShouldBe
        }

        Component { id: rowNumberSizeCalcComponent; TextMetrics { } }

        signal dataTableDoubleClicked()

        onDoubleClicked:  dataSetTableView.dataTableDoubleClicked()


        headerDelegate: Rectangle
        {
            //Two rectangles to show a border of exactly 1px around cells
            id: headerBorderRectangle
            color: systemPalette.mid
            border.width: 0
            radius: 0
            height: __myRoot.__headerHeight

            property real iconTextPadding: 10

            Rectangle
            {
                id: colHeader
                //gradient: headersGradient
                LinearGradient
                {
                    cached: true
                    anchors.fill: parent
                    start: Qt.point(colHeader.width * 0.5, 0)
                    end: Qt.point(colHeader.width * 0.5, colHeader.height)
                    gradient: headersGradient
                }

                /*readonly property bool isFirst: styleData.column == 0
                x: isFirst ? headerBorderRectangle.x - dataSetTableView.extraSpaceLeft : headerBorderRectangle.x
                width: isFirst ? headerBorderRectangle.width - 1 + dataSetTableView.extraSpaceLeft : headerBorderRectangle.width - 1*/

                x: headerBorderRectangle.x
                width: headerBorderRectangle.width - 1

                y: headerBorderRectangle.y
                height: headerBorderRectangle.height - 1


                Image
                {
                    id: colIcon
                    anchors.top: colHeader.top
                    anchors.bottom: colHeader.bottom
                    anchors.left: colHeader.left
                    anchors.leftMargin: 4

                    property int myColumnType: dataSetModel.columnIcon(styleData.column)
                    source: myColumnType >= 0 ? dataSetTableView.columnTypeAndIconPath[myColumnType] : ""
                    width: styleData.column > -1 ? __myRoot.__iconDim : 0
                    height:  __myRoot.__iconDim


                    function setColumnType(columnType)
                    {
                        colIcon.myColumnType = dataSetModel.setColumnTypeFromQML(styleData.column, columnType)

                        if(variablesWindow.chosenColumn == styleData.column && colIcon.myColumnType == columnTypes.columnTypeScale)
                            variablesWindow.chooseColumn(-1)


                    }

                    MouseArea
                    {
                        anchors.fill: parent
                        onClicked: if(styleData.column > -1) popupIcons.open()
                    }


                    Popup {
                        id: popupIcons; modal: true; focus: true;
                        padding: 2 * 1
                        y: colIcon.y + colIcon.height
                        x: colIcon.x - (__myRoot.__iconDim * 0.5)

                        closePolicy: Popup.CloseOnPressOutside | Popup.CloseOnEscape

                        TextMetrics { id: nominalTextMeasure; text: "Nominal"}

                        Column
                        {
                            width: parent.width
                            spacing: popupIcons.padding / 2

                            Repeater{
                                id: iconRepeater
                                model: dataSetModel.getColumnTypesWithCorrespondingIcon(false)

                                Button
                                {
                                    id: columnTypeChangeIcon
                                    iconSource: iconRepeater.model[index]
                                    width: __myRoot.__iconDim * 2.5 + nominalTextMeasure.width
                                    //anchors.left: parent.left
                                    //anchors.right: parent.right

                                    readonly property bool showThisTypeIcon: true // !((index == colIcon.myColumnType) || (index == columnTypes.columnTypeNominal && colIcon.myColumnType == columnTypes.columnTypeNominalText))
                                    height: showThisTypeIcon ? __myRoot.__iconDim * 1.5 : 0

                                    text: index == columnTypes.columnTypeScale ? "Scale" : ( index == columnTypes.columnTypeOrdinal ? "Ordinal" : "Nominal")


                                    onClicked: columnTypeChosen()

                                    function columnTypeChosen()
                                    {
                                        var columnType = index
                                        popupIcons.close()
                                        colIcon.setColumnType(columnType)
                                        filterWindow.sendFilter()

                                    }

                                }
                            }
                        }
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
                    leftPadding: headerBorderRectangle.iconTextPadding

                    MouseArea
                    {
                        anchors.fill: parent
                        onClicked:
                        {
                            var chooseThisColumn = (styleData.column > -1 && dataSetModel.columnIcon(styleData.column)  != columnTypes.columnTypeScale) ? styleData.column : -1
                            variablesWindow.chooseColumn(chooseThisColumn)

                        }
                    }
                }
            }
        }


        itemDelegate: Item
        {
            id: itemItem

            readonly property bool rowIsUsed: dataSetModel.getRowFilter(styleData.row)

            Rectangle
            {
                id: borderRectangle
                color: "transparent"
                border.width:   dataSetTableView.drawCellBorders ? 1 : 0
                border.color:  systemPalette.mid
                radius: 0
                width: parent.width + 1
                height: parent.height + 1
                x: parent.x - 1
                y: parent.y - 1
                visible: itemItem.rowIsUsed

            }

            Text {
                id: itemText
                text: styleData.value
                color: itemItem.rowIsUsed ? systemPalette.text : systemPalette.mid
                elide: styleData.elideMode
                horizontalAlignment: styleData.textAlignment
                leftPadding: 4
                anchors.verticalCenter: parent.verticalCenter

            }
        }

        rowDelegate: Item
        {
            height: 30
            Rectangle
            {
                color: styleData.selected ? systemPalette.dark :  (styleData.alternate && dataSetModel.getRowFilter(styleData.row) ? systemPalette.midlight : systemPalette.light)
                height: parent.height - 1
                anchors.left: parent.left
                anchors.right: parent.right
            }
        }

        rowNumberDelegate: Rectangle
        {
            id: rowNumberBorder
            anchors.left: parent.left
            anchors.top: parent.top
            //anchors.bottom: parent.bottom
            color: systemPalette.mid

            Rectangle
            {
                width: rowNumberBorder.width - 1
                height: rowNumberBorder.height - 1

                LinearGradient
                {
                    cached: true
                    anchors.fill: parent
                    start: Qt.point(0, rowNumberBorder.height * 0.5)
                    end: Qt.point(rowNumberBorder.width, rowNumberBorder.height * 0.5)
                    gradient: headersGradient
                }

                Text
                {
                    anchors.verticalCenter: parent.verticalCenter
                    text: styleData.row + 1
                    color: systemPalette.text
                }
            }
        }
    }

    Rectangle
    {
        id: dataStatusBar
        objectName: "dataStatusBar"
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.bottom: parent.bottom
        color: systemPalette.window

        height: datafiltertatusText.text.length > 0 ? datafiltertatusText.contentHeight : 0

        function setText(newText) { datafiltertatusText.text = newText }

        Text
        {
            id: datafiltertatusText
            text: ""
            anchors.fill: parent



        }
    }
}
