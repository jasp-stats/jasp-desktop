import QtQuick 2.7
import QtQuick.Controls 2.2 as New
import QtQuick.Controls 1.4
import QtQuick.Layouts 1.3

FocusScope {
	height: calculatedMinimumHeight
    visible: opened

	property real calculatedMinimumHeight: buttonColumnVariablesWindow.minimumHeight + columnNameVariablesWindow.height + 6

	Layout.minimumHeight: calculatedMinimumHeight


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

					anchors.rightMargin: 2

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
						var minimumWidth = calculateMinimumRequiredColumnWidthTitle(1, title, 0, 0)
						levelsTableViewValueColumn.width = minimumWidth + 10

					}

					function resizeLabelColumn()
					{
						/*var title = "Labels!"
						var minimumWidth = calculateMinimumRequiredColumnWidthTitle(2, title, 0, 0)
						levelsTableViewLabelColumn.width = minimumWidth + 10*/

					}

                    function moveUp()
                    {
						levelsTableViewRectangle.focus = true
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
						levelsTableViewRectangle.focus = true
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
						levelsTableViewRectangle.focus = true
                        copySelection()
                        levelsTableModel.reverse()
                        selection.clear()
                        var maxSelect = levelsTableModel.rowCount() - 1

                        for(var i=0; i<copiedSelection.length; i++)
                        {
                            var selectThis = maxSelect - copiedSelection[i]
                            selection.select(selectThis, selectThis)
                        }

						sendCurrentColumnChanged()
                    }

                    function closeYourself() { variablesWindow.chooseColumn(-1) }

                    TableViewColumn
                    {
                        id: levelsTableViewFilterColumn
                        title: "Filter"
						width: 40
                        role: "filter"
                    }


                    TableViewColumn
                    {
                        id: levelsTableViewValueColumn
                        title: "Value"
                        role: "value"
						width: 100
						//width: levelsTableView.width - levelsTableViewLabelColumn.width - 20 - levelsTableViewFilterColumn.width
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
						height: headerTextVars.contentHeight + 8
						//width: headerTextVars.width + 8

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
								font.pixelSize: baseFontSize * ppiScale

								anchors.verticalCenter: parent.verticalCenter
								x: 4
                            }
                        }
                    }

					rowDelegate: null

					itemDelegate: Rectangle
					{
						color: levelsTableView.selection.timesUpdated, levelsTableView.selection.contains(styleData.row) ? systemPalette.dark : (styleData.row % 2 == 1 ? systemPalette.midlight : systemPalette.light)

						height: 30 * ppiScale

						New.Button
						{
							id: filterCheckButton
							checkable: true
							visible: styleData.column === 0

							anchors.top: parent.top
							anchors.bottom: parent.bottom
							anchors.horizontalCenter: parent.horizontalCenter

							width: height

							checked: styleData.value


							onClicked:
								if(checked !== styleData.value)
									 if (!levelsTableModel.setAllowFilterOnLabel(styleData.row, checked))
                                         checked = true;

							background: Image
							{
								source: filterCheckButton.checked ? "qrc:/icons/check-mark.png" : "../images/cross.png"
								sourceSize.width: Math.max(40, width)
								sourceSize.height: Math.max(40, height)
								width:	filterCheckButton.width
								height: filterCheckButton.height

							}

						}

						Text {
							visible: styleData.column === 1

							color:			systemPalette.text
							text:			styleData.value
							elide:			Text.ElideMiddle
							font.pixelSize: baseFontSize * ppiScale
							anchors.fill:	parent
						}

						TextInput {
							visible:		styleData.column === 2

							color:			systemPalette.text

							text:			styleData.value
							font.pixelSize: baseFontSize * ppiScale
							clip:			true
							selectByMouse:	true
							autoScroll:		true

							anchors.fill:	parent
							function acceptChanges()
							{
								if(styleData.row >= 0 && styleData.column >= 0)
									levelsTableModel.setData(levelsTableModel.index(styleData.row, styleData.column), text)
							}
							onEditingFinished: focus = false

							onActiveFocusChanged:
								if(activeFocus)
								{
									levelsTableView.selection.clear()
									levelsTableView.selection.select(styleData.row, styleData.row)
								}
								else
								{
									if(focus)
										focus = false
									acceptChanges()
								}




							MouseArea
							{
								anchors.fill: parent
								acceptedButtons: Qt.NoButton
								cursorShape: Qt.IBeamCursor
							}
						}
					}
                }

                ColumnLayout
                {
									id:					buttonColumnVariablesWindow

									anchors.top:		parent.top
									anchors.right:		parent.right
									anchors.bottom:		parent.bottom
									spacing:			Math.max(1, 2 * ppiScale)
					property int	shownButtons:		4 + (eraseFiltersOnThisColumn.visible ? 1 : 0) + (eraseFiltersOnAllColumns.visible ? 1 : 0)
					property real	minimumHeight:		(buttonHeight + spacing) * shownButtons + (3 * spacing)
					property real	buttonHeight:		26 * ppiScale

					FilterButton
                    {
                        //text: "UP"
						iconSource: "../images/arrow-up.png"

						onClicked:		levelsTableView.moveUp()
						toolTip:		"Move selected labels up"

						height:			buttonColumnVariablesWindow.buttonHeight
						implicitHeight: buttonColumnVariablesWindow.buttonHeight
						width:			height
                    }

					FilterButton
                    {
                        //text: "DOWN"
						iconSource:		"../images/arrow-down.png"

						onClicked:		levelsTableView.moveDown()
						toolTip:		"Move selected labels down"

						height:			buttonColumnVariablesWindow.buttonHeight
						implicitHeight: buttonColumnVariablesWindow.buttonHeight
						width:			height
                    }

					FilterButton
					{
						//text: "REVERSE"
						iconSource:		"../images/arrow-reverse.png"
						onClicked:		levelsTableView.reverse()

						toolTip:		"Reverse order of all labels"

						height:			buttonColumnVariablesWindow.buttonHeight
						implicitHeight: buttonColumnVariablesWindow.buttonHeight
						width:			height
					}

					FilterButton
					{
						id:				eraseFiltersOnThisColumn
						iconSource:		"../images/eraser.png"
						onClicked:		levelsTableModel.resetFilterAllows()
						visible:		levelsTableModel.filteredOut > 0

						toolTip:		"Reset all filter checkmarks for this column"

						height:			buttonColumnVariablesWindow.buttonHeight
						implicitHeight: buttonColumnVariablesWindow.buttonHeight
						width:			height
					}

					FilterButton
					{
						id:				eraseFiltersOnAllColumns
						iconSource:		"../images/eraser_all.png"
						onClicked:		dataSetModel.resetAllFilters()
						visible:		dataSetModel.columnsFilteredCount > (levelsTableModel.filteredOut > 0 ? 1 : 0)
						height:			buttonColumnVariablesWindow.buttonHeight
						implicitHeight: buttonColumnVariablesWindow.buttonHeight
						width:			height

						toolTip:		"Reset all filter checkmarks for all columns"
					}

                    Item //Spacer
                    {
                        Layout.fillHeight: true
                    }

					FilterButton
                    {
						id:				variablesWindowCloseButton
						iconSource:		"../images/cross.png"
						onClicked:		variablesWindow.chooseColumn(-1)
						height:			buttonColumnVariablesWindow.buttonHeight
						implicitHeight: buttonColumnVariablesWindow.buttonHeight
						width:			height

						toolTip: "Close this view"
                    }
                }
            }

    }

}
