import QtQuick.Controls 2.1
import QtQuick.Controls.Styles 1.4
import QtQuick.Controls 1.4
import QtQuick 2.7
import QtQuick.Layouts 1.0

Rectangle {
    SystemPalette { id: systemPalette; colorGroup: SystemPalette.Active }
    id: rootDataset
    color: systemPalette.window

    Item
    {
        id: columnTypes
        //Copied from column.h, should actually be exported or something like that
        readonly property int columnTypeScale: 0
        readonly property int columnTypeOrdinal: 1
        readonly property int columnTypeNominal: 2
        readonly property int columnTypeNominalText: 3
    }

    Gradient{
        id: headersGradient
        GradientStop { position: 0.6; color: systemPalette.light }
        GradientStop { position: 0.7; color: systemPalette.midlight }
    }

    SplitView
    {
        anchors.fill: parent
        orientation: Qt.Vertical

        Rectangle {
            id: variablesWindow
            color: systemPalette.window
            height: 200
            visible: false

            property int chosenColumn: -1
            readonly property bool opened: chosenColumn != -1

            function chooseColumn(chooseThisColumn)
            {
                if(chosenColumn == chooseThisColumn) chooseThisColumn = -1
                chosenColumn = chooseThisColumn
                state = opened ? "opened" : " closed"

                if(chosenColumn > -1)
                {
                    columnNameVariablesWindow.text = dataSetModel.columnTitle(chosenColumn)
                    levelsTableModel.setColumnFromQML(chosenColumn)
                    levelsTableView.selection.clear()
                }
            }

            states: [
                State {
                    name: "closed"
                    PropertyChanges { target: variablesWindow; visible: false}
                },
                State {
                    name: "opened"
                    PropertyChanges { target: variablesWindow; visible: true}
                }
            ]

            Rectangle
            {
                anchors.fill: variablesWindow
                color: "transparent"

                Text
                {
                    id: columnNameVariablesWindow
                    text: "Column Name/Title here"
                    font.bold: true
                    anchors.top: parent.top
                    anchors.left: parent.left
                }

                Rectangle
                {
                    anchors.top: columnNameVariablesWindow.bottom
                    anchors.left: parent.left
                    anchors.right: parent.right
                    anchors.bottom: parent.bottom
                    anchors.bottomMargin: 6
                    anchors.topMargin: 6
                    color: "transparent"

                    TableViewJasp
                    {
                        id: levelsTableView
                        objectName: "levelsTableView"
                        anchors.top: parent.top
                        anchors.left: parent.left
                        anchors.right: buttonColumnVariablesWindow.left
                        anchors.bottom: parent.bottom

                        model: levelsTableModel

                        selectionMode: SelectionMode.ExtendedSelection

                        property var copiedSelection: []

                        function copySelection()
                        {
                            copiedSelection = []
                            selection.forEach( function(rowIndex) { copiedSelection.push(rowIndex) } )
                        }

                        function copySelectionReversed()
                        {
                            copiedSelection = []
                            var nonreversedSelectionCopy = []
                            selection.forEach( function(rowIndex) { nonreversedSelectionCopy.push(rowIndex) } )
                            for(var i=nonreversedSelectionCopy.length - 1; i >= 0; i--)
                                copiedSelection.push(nonreversedSelectionCopy[i])
                        }

                        TableViewColumn
                        {
                            id: levelsTableViewValueColumn
                            title: "Value"
                            role: "column_0"
                            width: 150
                        }

                        TableViewColumn
                        {
                            id: levelsTableViewLabelColumn
                            title: "Label"
                            role: "column_1"
                            width: levelsTableView.width - levelsTableViewValueColumn.width - 10
                        }

                        headerDelegate: Rectangle
                        {
                            //Two rectangles to show a border of exactly 1px around cells
                            id: headerBorderRectangleVars
                            color: systemPalette.mid
                            border.width: 0
                            radius: 0
                            height: headerTextVars.implicitHeight
                            width: headerTextVars.width

                            Rectangle
                            {
                                id: colHeaderVars
                                gradient: headersGradient

                                x: headerBorderRectangleVars.x
                                y: headerBorderRectangleVars.y
                                height: headerBorderRectangleVars.height - 1
                                width: headerBorderRectangleVars.width - 1

                                Text
                                {
                                    id: headerTextVars
                                    text: styleData.value
                                }
                            }
                        }

                    }

                    ColumnLayout
                    {
                        id: buttonColumnVariablesWindow

                        anchors.top: parent.top
                        anchors.right: parent.right
                        anchors.bottom: parent.bottom

                        Button
                        {
                            //text: "UP"
                            iconSource: "../images/arrow-up.png"

                            onClicked:
                            {
                                levelsTableView.copySelection()
                                if(levelsTableView.copiedSelection.length > 0 && levelsTableView.copiedSelection[0] != 0)
                                {
                                    levelsTableModel.moveUpFromQML(levelsTableView.copiedSelection)

                                    levelsTableView.selection.clear()

                                    for(var i=0; i<levelsTableView.copiedSelection.length; i++)
                                    {
                                        var selectThis = levelsTableView.copiedSelection[i]
                                        if(selectThis > 0)
                                            levelsTableView.selection.select(selectThis - 1, selectThis - 1)
                                    }
                                }
                            }
                        }

                        Button
                        {
                            //text: "DOWN"
                            iconSource: "../images/arrow-down.png"

                            onClicked:
                            {
                                levelsTableView.copySelectionReversed()
                                if(levelsTableView.copiedSelection.length > 0 && (levelsTableView.copiedSelection[0] != (levelsTableModel.rowCount() - 1)))
                                {
                                    levelsTableModel.moveDownFromQML(levelsTableView.copiedSelection)

                                    levelsTableView.selection.clear()

                                    for(var i=0; i<levelsTableView.copiedSelection.length; i++)
                                    {
                                        var selectThis = levelsTableView.copiedSelection[i]

                                        if(selectThis < levelsTableModel.rowCount() - 1)
                                            levelsTableView.selection.select(selectThis + 1, selectThis + 1)
                                    }
                                }
                            }
                        }

                        Button
                        {
                            //text: "REVERSE"
                            iconSource: "../images/arrow-reverse.png"
                            onClicked:
                            {
                                levelsTableView.copySelection()
                                levelsTableModel.reverse()
                                levelsTableView.selection.clear()
                                var maxSelect = levelsTableModel.rowCount() - 1

                                for(var i=0; i<levelsTableView.copiedSelection.length; i++)
                                {
                                    var selectThis = maxSelect - levelsTableView.copiedSelection[i]
                                    levelsTableView.selection.select(selectThis, selectThis)
                                }

                            }
                        }

                        Item //Spacer
                        {
                            Layout.fillHeight: true
                        }

                        Button
                        {
                            id: variablesWindowCloseButton
                            iconSource: "../images/cross.png"
                            onClicked: variablesWindow.chooseColumn(-1)
                        }
                    }
                }
            }
        }

        TableViewJasp {
            id: dataSetTableView
            objectName: "dataSetTableView"

            Layout.fillWidth: true

            /*anchors.top: variablesWindow.bottom
            anchors.left: rootDataset.left
            anchors.right: rootDataset.right
            anchors.bottom: rootDataset.bottom*/

            alternatingRowColors: true
            property bool drawCellBorders: true


            model: dataSetModel

            readonly property var columnTypeAndIconPath: dataSetModel.getColumnTypesWithCorrespondingIcon(true)
            readonly property var columnTypeChangeIconPaths:dataSetModel.getColumnTypesWithCorrespondingIcon(false)

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

            signal dataTableDoubleClicked()

            MouseArea
            {
                anchors.fill: parent
                propagateComposedEvents: true
                onClicked: mouse.accepted = false
                onDoubleClicked: dataSetTableView.dataTableDoubleClicked()
            }

            headerDelegate: Rectangle
            {
                //Two rectangles to show a border of exactly 1px around cells
                id: headerBorderRectangle
                color: systemPalette.mid
                border.width: 0
                radius: 0
                height: headerBorderRectangle.iconDim * 1.2

                property real iconDim: headerText.implicitHeight
                property real iconTextPadding: 10

                Rectangle
                {
                    id: colHeader
                    gradient: headersGradient

                    x: headerBorderRectangle.x
                    y: headerBorderRectangle.y
                    height: headerBorderRectangle.height - 1
                    width: headerBorderRectangle.width - 1

                    Image
                    {
                        id: colIcon
                        anchors.top: colHeader.top
                        anchors.bottom: colHeader.bottom
                        anchors.left: colHeader.left
                        anchors.leftMargin: 4

                        property int myColumnType: dataSetModel.columnIcon(styleData.column)
                        source: dataSetTableView.columnTypeAndIconPath[myColumnType]
                        width: styleData.column > -1 ? headerBorderRectangle.iconDim : 0
                        height:  headerBorderRectangle.iconDim


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
                            id: popupIcons; modal: true; focus: true; padding: 5
                            y: colIcon.y + colIcon.height
                            x: colIcon.x - (headerBorderRectangle.iconDim * 0.5)

                            closePolicy: Popup.CloseOnPressOutside | Popup.CloseOnEscape

                            Column
                            {
                                width: parent.width
                                spacing: popupIcons.padding

                                Repeater{
                                    id: iconRepeater
                                    model: dataSetModel.getColumnTypesWithCorrespondingIcon(false)

                                    Button
                                    {
                                        id: columnTypeChangeIcon
                                        iconSource: iconRepeater.model[index]
                                        width: headerBorderRectangle.iconDim * 1.5
                                        readonly property bool showThisTypeIcon:  !((index == colIcon.myColumnType) || (index == columnTypes.columnTypeNominal && colIcon.myColumnType == columnTypes.columnTypeNominalText))
                                        height: showThisTypeIcon ? headerBorderRectangle.iconDim * 1.5 : 0


                                        onClicked: columnTypeChosen()

                                        function columnTypeChosen()
                                        {
                                            var columnType = index
                                            popupIcons.close()
                                            colIcon.setColumnType(columnType)

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
}
