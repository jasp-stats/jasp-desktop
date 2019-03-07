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


import QtQuick 2.11
import QtQuick.Controls 2.4
import QtQuick.Layouts 1.3
import JASP.Controls 1.0
import JASP.Theme 1.0
import QtQuick.Window 2.3
import JASP 1.0

JASPControl
{
	id:					tableView

	controlType:		"TableView"
	activeFocusOnTab:	false

	property var	source
	property alias	syncModels:	tableView.source
	property string	modelType
	property string	itemType:	"string"
	property string	tableType
	property alias	model:		theView.model
	property var	validator:	(itemType === "integer") ? intValidator : (itemType === "double" ? doubleValidator : stringValidator)
	property int	colSelected: -1
	property int	columnCount: 0
	property int	rowCount: 0

	signal addColumn();
	signal removeColumn(int col);
	signal reset();
	signal itemChanged(int col, int row, string value)

	function removeAColumn()
	{
		if (colSelected >= 0)
			removeColumn(colSelected);
	}

	Rectangle
	{
		id:					rectangleBorder
		anchors.centerIn:	parent
		width:				parent.width
		height:				parent.height
		border.width:		1
		border.color:		Theme.borderColor
		color:				Theme.white
	}

	Flickable
	{
		id:				myFlickable
		anchors.fill:	parent
		contentWidth:	theView.width
		contentHeight:	theView.height
		clip:			true


		DataSetView
		{
			z:						-1
			id:						theView
			model:					null
			itemHorizontalPadding:	0
			itemVerticalPadding:	8 * preferencesModel.uiScale

			viewportX: myFlickable.visibleArea.xPosition * width
			viewportY: myFlickable.visibleArea.yPosition * height
			viewportW: myFlickable.visibleArea.widthRatio * width
			viewportH: myFlickable.visibleArea.heightRatio * height

			columnHeaderDelegate: Rectangle
			{
				color: columnIndex === tableView.colSelected ? Theme.grayLighter : Theme.analysisBackgroundColor
				Text { text: headerText; anchors.centerIn: parent }
				MouseArea
				{
					anchors.fill: parent
					onClicked: {
						if (tableView.colSelected === columnIndex)
							columnIndex = -1
						tableView.colSelected = columnIndex;
					}
				}
			}

			rowNumberDelegate: Rectangle
			{
				color: Theme.analysisBackgroundColor
				Text
				{
					text:					headerText;
					anchors.centerIn:		parent;
					horizontalAlignment:	Text.AlignHCenter
					verticalAlignment:		Text.AlignVCenter
					leftPadding:			3 * preferencesModel.uiScale
					elide:					Text.ElideRight;
					width:					parent.width
					height:					parent.width
				}

				ToolTip.visible:			mouseAreaItem.containsMouse
				ToolTip.delay:				Theme.toolTipDelay
				ToolTip.timeout:			Theme.toolTipTimeout
				ToolTip.text:				headerText

				MouseArea
				{
					id:				mouseAreaItem
					hoverEnabled:	true
					anchors.fill:	parent
				}
			}

			JASPDoubleValidator	{ id: intValidator; bottom: 0; decimals: 0 }
			JASPDoubleValidator { id: doubleValidator; bottom: 0; decimals: 1 }
			RegExpValidator { id: stringValidator }

			itemDelegate: Rectangle
			{
				TextField
				{
					fieldWidth:			parent.width
					fieldHeight:		parent.height
					value:				itemText;
					inputType:			tableView.itemType
					useExternalBorder:	false
					isBound:			false
					validator:			tableView.validator
					onPressed:			tableView.colSelected = columnIndex
					onEditingFinished:	tableView.itemChanged(columnIndex, rowIndex, value)
				}
			}

			leftTopCornerItem: Rectangle { color: Theme.analysisBackgroundColor }

		}

		ScrollBar.vertical:		ScrollBar   { z: 0; stepSize: 0.025 }
		ScrollBar.horizontal:	ScrollBar	{ z: 0; stepSize: 0.025	}
	}
}
