import QtQuick.Controls 2.1
import QtQuick.Controls.Styles 1.4
import QtQuick.Controls 1.4
import QtQuick 2.7
import QtQuick.Layouts 1.0
import QtGraphicalEffects 1.0

Rectangle {
    SystemPalette { id: systemPalette; colorGroup: SystemPalette.Active }
    id: rootDataset
    color: systemPalette.window
    readonly property int iconDim: 16
    readonly property int headerHeight: 20

    Item
    {
        id: columnTypes
        //Copied from column.h, should actually be exported or something like that
        readonly property int columnTypeScale: 0
        readonly property int columnTypeOrdinal: 1
        readonly property int columnTypeNominal: 2
        readonly property int columnTypeNominalText: 3
    }

    //Fancy pants curvy gradient for the columnheaders and rownumbers.
    Gradient{
        id: headersGradient
        GradientStop { position: 0.2; color: systemPalette.midlight }
        GradientStop { position: 0.5; color: systemPalette.light }
        GradientStop { position: 0.9; color: systemPalette.midlight }
        GradientStop { position: 1.0; color: systemPalette.mid }

    }


    Rectangle
    {
        id: progressBarHolder
        objectName: "progressBarHolder"
        color: systemPalette.midlight
        visible: false
        anchors.left: rootDataset.left
        anchors.right: rootDataset.right
        anchors.top: rootDataset.top

        Text
        {
            id: loadingText
            horizontalAlignment: Text.AlignHCenter
            text: "Say something about progress here ;)"

            anchors.left: parent.left
            anchors.right: parent.right
            anchors.top: parent.top
        }


        ProgressBar
        {
            id: loadingBar
            minimumValue: 0
            maximumValue: 100
            value: 0


            anchors.left: progressBarHolder.left
            anchors.right: progressBarHolder.right
            anchors.top: loadingText.top
            anchors.topMargin: 24
        }

        function show()
        {
            setStatus("", 0)
            visible = true
            splitViewData.visible = false
        }

        function hide()
        {
            visible = false
            splitViewData.visible = true
        }

        function setStatus(statusString, progress)
        {
            loadingText.text = statusString
            loadingBar.value = progress

        }

    }

    SplitView
    {
        id: splitViewData
        //anchors.fill: parent
        anchors.top: progressBarHolder.bottom
        anchors.left: rootDataset.left
        anchors.right: rootDataset.right
        anchors.bottom: rootDataset.bottom

        orientation: Qt.Vertical
        visible: false

        Rectangle {
            id: variablesWindow
            color: systemPalette.window
            height: 200
            visible: false

            Layout.minimumHeight: 100

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

                        signal columnChanged(string columnName)

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

                        function sendCurrentColumnChanged()
                        {
                            if(variablesWindow.chosenColumn > -1)
                                columnChanged(dataSetModel.columnTitle(variablesWindow.chosenColumn))
                        }

                        function resizeValueColumn()
                        {
                            var title = "Values!"
                            var minimumWidth = calculateMinimumRequiredColumnWidthTitle(0, title, 0, 0)
                            levelsTableViewValueColumn.width = minimumWidth + 10

                        }

                        function moveUp()
                        {
                            copySelection()
                            if(copiedSelection.length > 0 && copiedSelection[0] != 0)
                            {
                                levelsTableModel.moveUpFromQML(copiedSelection)

                                selection.clear()

                                for(var i=0; i<copiedSelection.length; i++)
                                {
                                    var selectThis = copiedSelection[i]
                                    if(selectThis > 0)
                                        selection.select(selectThis - 1, selectThis - 1)
                                }
                            }

                            sendCurrentColumnChanged()
                        }

                        function moveDown()
                        {
                            copySelectionReversed()
                            if(copiedSelection.length > 0 && (copiedSelection[0] != (levelsTableModel.rowCount() - 1)))
                            {
                                levelsTableModel.moveDownFromQML(copiedSelection)

                                selection.clear()

                                for(var i=0; i<copiedSelection.length; i++)
                                {
                                    var selectThis = copiedSelection[i]

                                    if(selectThis < levelsTableModel.rowCount() - 1)
                                        selection.select(selectThis + 1, selectThis + 1)
                                }
                            }

                            sendCurrentColumnChanged()
                        }

                        function reverse()
                        {
                            copySelection()
                            levelsTableModel.reverse()
                            selection.clear()
                            var maxSelect = levelsTableModel.rowCount() - 1

                            for(var i=0; i<copiedSelection.length; i++)
                            {
                                var selectThis = maxSelect - copiedSelection[i]
                                selection.select(selectThis, selectThis)
                            }

                        }

                        function closeYourself() { variablesWindow.chooseColumn(-1) }



                        TableViewColumn
                        {
                            id: levelsTableViewValueColumn
                            title: "Value"
                            role: "column_0"
                        }

                        TableViewColumn
                        {
                            id: levelsTableViewLabelColumn
                            title: "Label"
                            role: "column_1"
                            width: levelsTableView.width - levelsTableViewValueColumn.width - 20
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
                                    color: systemPalette.text
                                }
                            }
                        }

                        itemDelegate: Loader
                        {
                            property string textItem: styleData.value
                            property color colorItem: systemPalette.text
                            property int rowItem: styleData.row
                            property int colItem: styleData.column

                            sourceComponent: styleData.column == 1 ? textInputVariablesWindowTemplate : textDisplayVariablesWindowTemplate
                        }


                        Component {
                            id: textInputVariablesWindowTemplate
                            TextInput {
                                color: colorItem
                                text: textItem


                                anchors.fill: parent
                                function acceptChanges() { levelsTableModel.setData(levelsTableModel.index(rowItem, colItem), text) }
                                onEditingFinished: acceptChanges()
                            }
                        }

                        Component {
                            id: textDisplayVariablesWindowTemplate
                            Text {
                                color: colorItem
                                text: textItem
                                elide: Text.ElideMiddle
                                anchors.fill: parent

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

                            onClicked: levelsTableView.moveUp()
                        }

                        Button
                        {
                            //text: "DOWN"
                            iconSource: "../images/arrow-down.png"

                            onClicked: levelsTableView.moveDown()
                        }

                        Button
                        {
                            //text: "REVERSE"
                            iconSource: "../images/arrow-reverse.png"
                            onClicked: levelsTableView.reverse()

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

        SplitView
        {
            id: filterWindow

            property int minimumHeightTextBoxes: 50
            property string lastAppliedFilter: ""
            height: filterWindow.minimumHeightTextBoxes * 3
            Layout.minimumHeight: filterWindow.minimumHeightTextBoxes + applyFilter.implicitHeight + (filterError.visible ? filterWindow.minimumHeightTextBoxes : 0 )
            orientation: Qt.Vertical
            visible: false



            property bool opened: false

            function toggle()
            {
                opened = !opened
                state = opened ? "opened" : "closed"
                filterEdit.text = engineSync.getFilter()
            }

            function sendFilter()
            {
                engineSync.sendFilter(lastAppliedFilter)
            }

            function applyAndSendFilter(newFilter)
            {
                lastAppliedFilter = newFilter
                sendFilter()
            }

            states: [
                State {
                    name: "closed"
                    PropertyChanges { target: filterWindow; visible: false}
                    when: !opened
                },
                State {
                    name: "opened"
                    PropertyChanges { target: filterWindow; visible: true}
                    when: opened
                }
            ]

            Rectangle {
                id: filterEditPlusButton
                color: systemPalette.base

                border.width: 1
                border.color: systemPalette.mid
                Layout.fillHeight: true
                Layout.minimumHeight: applyFilter.height + filterWindow.minimumHeightTextBoxes

                TextArea
                {

                    id: filterEdit
                    anchors.top: parent.top
                    anchors.left: parent.left
                    anchors.right: parent.right
                    anchors.bottom: applyFilter.top
                }

                Button
                {
                    id: applyFilter
                    text: "Apply Filter"
                    anchors.horizontalCenter: parent.horizontalCenter
                    anchors.bottom: parent.bottom
                    onClicked: filterWindow.applyAndSendFilter(filterEdit.text)

                }

                Button
                {
                    id: closeFilterButton
                    iconSource: "../images/cross.png"
                    anchors.right: parent.right
                    anchors.bottom: parent.bottom
                    onClicked: filterWindow.toggle()

                }
            }

            TextArea
            {
                id: filterError
                textColor: "red"
                readOnly: true
                text: filterErrorText
                Layout.minimumHeight: Math.min(filterError.contentHeight, filterWindow.minimumHeightTextBoxes)

                states: [
                    State {
                        name: "closed"
                        PropertyChanges { target: filterError; visible: false; height: 0 }
                        when: filterError.text.length == 0
                    },
                    State {
                        name: "opened"
                        PropertyChanges { target: filterError; visible: true; height: Math.min(filterError.contentHeight, filterWindow.minimumHeightTextBoxes)}

                        when: filterError.text.length > 0
                    }
                ]
            }


        }

        Rectangle
        {
            Layout.fillHeight: true

            Rectangle
            {
                width: dataSetTableView.extraSpaceLeft + 1
                height: rootDataset.headerHeight + 1
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
                    width: rootDataset.iconDim
                    height: rootDataset.iconDim
                    x: parent.x + (parent.width / 2) - (width / 2)
                    y: parent.y + (parent.height / 2) - (height / 2)
                }

                MouseArea
                {
                    anchors.fill: parent

                    onClicked: filterWindow.toggle()
                }
            }

            TableViewJasp {

                id: dataSetTableView
                objectName: "dataSetTableView"

                anchors.fill: parent

                alternatingRowColors: false
                property bool drawCellBorders: true


                model: dataSetModel

                readonly property var columnTypeAndIconPath: dataSetModel.getColumnTypesWithCorrespondingIcon(true)
                readonly property var columnTypeChangeIconPaths:dataSetModel.getColumnTypesWithCorrespondingIcon(false)

                function reloadColumns()
                {
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
                    height: rootDataset.headerHeight

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
                            source: dataSetTableView.columnTypeAndIconPath[myColumnType]
                            width: styleData.column > -1 ? rootDataset.iconDim : 0
                            height:  rootDataset.iconDim


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
                                x: colIcon.x - (rootDataset.iconDim * 0.5)

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
                                            width: rootDataset.iconDim * 2.5 + nominalTextMeasure.width
                                            //anchors.left: parent.left
                                            //anchors.right: parent.right

                                            readonly property bool showThisTypeIcon: true // !((index == colIcon.myColumnType) || (index == columnTypes.columnTypeNominal && colIcon.myColumnType == columnTypes.columnTypeNominalText))
                                            height: showThisTypeIcon ? rootDataset.iconDim * 1.5 : 0

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
                    //z: (rowIsUsed ? 1 : 0)
                    z: 10

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

                    }
                }

                rowDelegate: Item
                {
                    Rectangle
                    {
                        color: styleData.selected ? systemPalette.dark :  (styleData.alternate && dataSetModel.getRowFilter(styleData.row) ? systemPalette.midlight : systemPalette.light)
                        height: parent.height - 1
                    }
                }

                rowNumberDelegate: Rectangle
                {
                    id: rowNumberBorder
                    anchors.left: parent.left
                    anchors.top: parent.top
                    anchors.bottom: parent.bottom
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
                            text: styleData.row + 1
                            color: systemPalette.text
                        }
                    }
                }
            }
        }
    }
}
