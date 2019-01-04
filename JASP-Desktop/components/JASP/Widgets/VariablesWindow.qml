//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

import QtQuick 2.7
import QtQuick.Controls 2.2 as New
import QtQuick.Controls 1.4 as OLD
import QtQuick.Layouts 1.3

import JASP.Theme 1.0

FocusScope {
	height: calculatedMinimumHeight
    visible: opened

	property real calculatedMinimumHeight: buttonColumnVariablesWindow.minimumHeight + columnNameVariablesWindow.height + 6 + (Theme.generalAnchorMargin * 2)

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


	Item
	{
		id:					levelsTableViewRectangle
		anchors.fill:		parent
		anchors.margins:	Theme.generalAnchorMargin



            Text
            {
                id: columnNameVariablesWindow
                text: "Column Name/Title here"
                font.bold: true
                anchors.top: parent.top
                anchors.left: parent.left
            }

			Item
            {
				anchors
				{
					top:			columnNameVariablesWindow.bottom
					left:			parent.left
					right:			parent.right
					bottom:			parent.bottom
					bottomMargin:	6
					topMargin:		6
				}

				//color: "transparent"


				TableViewJasp
                {
					id:				levelsTableView
					objectName:		"levelsTableView"
					anchors
					{
						top:			parent.top
						left:			parent.left
						right:			buttonColumnVariablesWindow.left
						bottom:			parent.bottom
						rightMargin:	2
					}

                    signal columnChanged(string columnName)

                    model: levelsTableModel

					selectionMode: OLD.SelectionMode.ExtendedSelection

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


					OLD.TableViewColumn
                    {
                        id: levelsTableViewFilterColumn
                        title: "Filter"
						width: 60
                        role: "filter"
                    }


					OLD.TableViewColumn
                    {
                        id: levelsTableViewValueColumn
                        title: "Value"
                        role: "value"
						width: 120
						//width: levelsTableView.width - levelsTableViewLabelColumn.width - 20 - levelsTableViewFilterColumn.width
                    }

					OLD.TableViewColumn
                    {
                        id: levelsTableViewLabelColumn
                        title: "Label"
                        role: "label"
						width: levelsTableView.width - levelsTableViewValueColumn.width - 10 - levelsTableViewFilterColumn.width
                    }

                    headerDelegate: Rectangle
                    {
                        //Two rectangles to show a border of exactly 1px around cells
                        id: headerBorderRectangleVars
						color:			Theme.grayDarker
						border.width:	0
                        radius: 0
						height: headerTextVars.contentHeight + (Theme.itemPadding * 2)
						//width: headerTextVars.width + 8

                        Rectangle
                        {
                            id: colHeaderVars
							color:	Theme.uiBackground

                            x: headerBorderRectangleVars.x
                            y: headerBorderRectangleVars.y
                            height: headerBorderRectangleVars.height - 1
                            width: headerBorderRectangleVars.width - 1

                            Text
                            {
								id:		headerTextVars
								text:	styleData.value
								color:	Theme.textEnabled
								font:	Theme.font

								anchors.verticalCenter: parent.verticalCenter
								x:  Theme.itemPadding
                            }
                        }
                    }

					rowDelegate: Item { height: (30 * ppiScale)  }

					itemDelegate: Rectangle
					{
						color:			levelsTableView.selection.timesUpdated, levelsTableView.selection.contains(styleData.row) ? Theme.itemHighlight : "transparent"

						New.Button
						{
							id: filterCheckButton
							checkable: true
							visible: styleData.column === 0

							anchors
							{
								top:				parent.top
								bottom:				parent.bottom
								horizontalCenter:	parent.horizontalCenter
								margins:			4
							}

							width: height

							checked: styleData.value


							onClicked:
								if(checked !== styleData.value)
									 if (!levelsTableModel.setAllowFilterOnLabel(styleData.row, checked))
                                         checked = true;

							background: Image
							{
								source: filterCheckButton.checked ? "qrc:/icons/check-mark.png" : "qrc:/images/cross.png"
								sourceSize.width: Math.max(40, width)
								sourceSize.height: Math.max(40, height)
								width:	filterCheckButton.width
								height: filterCheckButton.height

							}

						}

						Text {
							visible: styleData.column === 1

							color:			Theme.textEnabled
							text:			styleData.value
							elide:			Text.ElideMiddle
							font.pixelSize: baseFontSize * ppiScale
							anchors.fill:	parent
							verticalAlignment: Text.AlignVCenter
						}

						TextInput {
							visible:		styleData.column === 2

							color:			Theme.textEnabled

							text:			styleData.value
							font.pixelSize: baseFontSize * ppiScale
							clip:			true
							selectByMouse:	true
							autoScroll:		true

							anchors.fill:	parent
							verticalAlignment: Text.AlignVCenter

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
					property real	buttonHeight:		32 * ppiScale

					RectangularButton
                    {
                        //text: "UP"
						iconSource:		"qrc:/images/arrow-up.png"

						onClicked:		levelsTableView.moveUp()
						toolTip:		"Move selected labels up"

						height:			buttonColumnVariablesWindow.buttonHeight
						implicitHeight: buttonColumnVariablesWindow.buttonHeight
						width:			height
                    }

					RectangularButton
                    {
                        //text: "DOWN"
						iconSource:		"qrc:/images/arrow-down.png"

						onClicked:		levelsTableView.moveDown()
						toolTip:		"Move selected labels down"

						height:			buttonColumnVariablesWindow.buttonHeight
						implicitHeight: buttonColumnVariablesWindow.buttonHeight
						width:			height
                    }

					RectangularButton
					{
						//text: "REVERSE"
						iconSource:		"qrc:/images/arrow-reverse.png"
						onClicked:		levelsTableView.reverse()

						toolTip:		"Reverse order of all labels"

						height:			buttonColumnVariablesWindow.buttonHeight
						implicitHeight: buttonColumnVariablesWindow.buttonHeight
						width:			height
					}

					RectangularButton
					{
						id:				eraseFiltersOnThisColumn
						iconSource:		"qrc:/images/eraser.png"
						onClicked:		levelsTableModel.resetFilterAllows()
						visible:		levelsTableModel.filteredOut > 0

						toolTip:		"Reset all filter checkmarks for this column"

						height:			buttonColumnVariablesWindow.buttonHeight
						implicitHeight: buttonColumnVariablesWindow.buttonHeight
						width:			height
					}

					RectangularButton
					{
						id:				eraseFiltersOnAllColumns
						iconSource:		"qrc:/images/eraser_all.png"
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

					RectangularButton
                    {
						id:				variablesWindowCloseButton
						iconSource:		"qrc:/images/cross.png"
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
