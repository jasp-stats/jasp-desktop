import QtQuick 2.7
import QtQuick.Controls 2.3 as New
import QtQuick.Controls 1.4
import QtQuick.Layouts 1.3

FocusScope {
    height: 200
    visible: opened

    Layout.minimumHeight: buttonColumnVariablesWindow.minimumHeight


    property var headersGradient: Gradient{
        GradientStop { position: 0.2; color: systemPalette.midlight }
        GradientStop { position: 1.0; color: systemPalette.light }
    }

    property int chosenColumn: -1
    readonly property bool opened: chosenColumn != -1

    function chooseColumn(chooseThisColumn)
    {
        if(chosenColumn == chooseThisColumn)
            chooseThisColumn = -1
        chosenColumn = chooseThisColumn

        if(chosenColumn > -1)
        {
            //to prevent the editText in the labelcolumn to get stuck and overwrite the next columns data... We have to remove activeFocus from it
            levelsTableViewRectangle.focus = true //So we just put it somewhere
            columnNameVariablesWindow.text = dataSetModel.columnTitle(chosenColumn)
            levelsTableModel.setColumnFromQML(chosenColumn)
            levelsTableView.selection.clear()
        }
    }


    Rectangle {
        id: levelsTableViewRectangle
        anchors.fill: parent
        color: systemPalette.window


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
                        id: levelsTableViewFilterColumn
                        title: "Filter"
                        width: 36
                        role: "filter"
                    }


                    TableViewColumn
                    {
                        id: levelsTableViewValueColumn
                        title: "Value"
                        role: "value"
                    }

                    TableViewColumn
                    {
                        id: levelsTableViewLabelColumn
                        title: "Label"
                        role: "label"
                        width: levelsTableView.width - levelsTableViewValueColumn.width - 20 - levelsTableViewFilterColumn.width
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
                        property var valueItem: styleData.value

                        sourceComponent: styleData.column == 0 ? filterCheckBoxVariablesWindowTemplate : (styleData.column == 2 ? textInputVariablesWindowTemplate : textDisplayVariablesWindowTemplate)

                        //anchors.fill: parent
                    }


                    Component {
                        id: textInputVariablesWindowTemplate
                        TextInput {
                            color: colorItem

                            text: textItem
                            clip: true
                            selectByMouse: true
                            autoScroll: true

                            anchors.fill: parent
                            function acceptChanges() { levelsTableModel.setData(levelsTableModel.index(rowItem, colItem), text) }
                            onEditingFinished: acceptChanges()

                            onActiveFocusChanged:
                                if(activeFocus)
                                {
                                    levelsTableView.selection.clear()
                                    levelsTableView.selection.select(rowItem, rowItem)
                                }
                                else if(focus)
                                    focus = false



                            MouseArea
                            {
                                anchors.fill: parent
                                acceptedButtons: Qt.NoButton
                                cursorShape: Qt.IBeamCursor
                            }
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

                    Component {
                        id: filterCheckBoxVariablesWindowTemplate
                        PlusMinusCheckButton
                        {
                            color: colorItem
                            checked: valueItem

                            id: filterCheckButton
                            onClicked:
                            {
                                if(checked != valueItem)
                                     levelsTableModel.setAllowFilterOnLabel(rowItem, checked);
                                checked = levelsTableModel.allowFilter(rowItem);
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
                    property real minimumHeight: (variablesWindowCloseButton.height + spacing) * 4 + ( 3 * spacing)

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
