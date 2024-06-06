//
// Copyright (C) 2013-2024 University of Amsterdam
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

import QtQuick
import QtQuick.Controls
import JASP.Controls

Popup
{
	id:			popupResizeData;
	modal:		true;

	y:			(parent.height / 2) - (height / 2)
	x:			(parent.width / 2)  - (width / 2)
	width:		popupLoader.width
	height:		popupLoader.height+1

	background: Rectangle
	{
		color:			jaspTheme.uiBackground
		border.color:	jaspTheme.uiBorder
		border.width:	1
		radius:			jaspTheme.borderRadius
	}
	padding:	0

	Connections
	{
		target:					ribbonModel
		function onResizeData() {	popupResizeData.open() }
	}

	Loader
	{
		id:					popupLoader
		sourceComponent:	visible ? resizeComp : null
		visible:			popupResizeData.opened
	}

	Component
	{
		id:	resizeComp

		Item
		{
			height:	inputs.height + jaspTheme.generalAnchorMargin + (30 * jaspTheme.uiScale)
			width:	250 * jaspTheme.uiScale

			Component.onCompleted:	cols.forceActiveFocus();

			Text
			{
				id:					title
				text:				qsTr("Resize Data")
				font:				jaspTheme.fontGroupTitle
				color:				jaspTheme.textEnabled
				verticalAlignment:	Text.AlignVCenter
				anchors
				{
					top:				parent.top
					topMargin:			jaspTheme.generalAnchorMargin
					horizontalCenter:	parent.horizontalCenter
				}
			}

			Item
			{
				id:				inputs
				height:			95 * jaspTheme.uiScale + 2 * jaspTheme.generalAnchorMargin

				anchors
				{
					top:		title.bottom
					left:		parent.left
					right:		parent.right
					margins:	jaspTheme.generalAnchorMargin
				}

				Label
				{
					id:				colsLabel
					text:			qsTr("Columns")
					focus:			true
					anchors
					{
						top:				inputs.top
						horizontalCenter:	cols.horizontalCenter
					}
				}

				Label
				{
					id:				rowsLabel
					text:			qsTr("Rows")
					anchors
					{
						top:				inputs.top
						horizontalCenter:	rows.horizontalCenter
					}
				}

				Label
				{
					id:						x
					text:					"X"
					horizontalAlignment:	Text.AlignHCenter
					verticalAlignment:		Text.AlignVCenter
					anchors
					{
						top:				colsLabel.bottom
						horizontalCenter:	parent.horizontalCenter
						topMargin:			jaspTheme.generalAnchorMargin * 0.5
					}
				}

				IntegerField
				{
					id:						cols
					defaultValue:			dataSetModel.columnCount()
					fieldWidth:				width
					selectValueOnFocus:		true
					anchors
					{
						top:				x.bottom
						left:				parent.left
						right:				x.left
						margins:			jaspTheme.generalAnchorMargin
						topMargin:			jaspTheme.generalAnchorMargin * 0.5
					}
					onEditingFinished:		rows.forceActiveFocus();

					KeyNavigation.tab:		rows
					KeyNavigation.right:	rows
					KeyNavigation.down:		resizeButton
				}

				IntegerField
				{
					id:						rows
					defaultValue:			dataSetModel.rowCount()
					fieldWidth:				width
					selectValueOnFocus:		true
					anchors
					{
						top:				x.bottom
						left:				x.right
						right:				parent.right
						margins:			jaspTheme.generalAnchorMargin
						topMargin:			jaspTheme.generalAnchorMargin * 0.5
					}

					onEditingFinished:		resizeButton.forceActiveFocus();
					KeyNavigation.down:		closeButtonCross
					KeyNavigation.tab:		resizeButton
				}
			}

			RoundedButton
			{
				id:						resizeButton
				activeFocusOnTab:		true
				text:					qsTr("Resize")
				onClicked:
				{
					mainWindow.resizeData(rows.displayValue, cols.displayValue);
					popupResizeData.close();
				}

				toolTip:				qsTr("Resize data to set values")

				KeyNavigation.tab:		closeButtonCross

				anchors
				{
					bottom:				inputs.bottom
					margins:			jaspTheme.generalAnchorMargin
					left:				parent.left
					right:				closeButtonCross.left
				}
			}

			RoundedButton
			{
				id:						closeButtonCross
				activeFocusOnTab:		true
				iconSource:				jaspTheme.iconPath + "cross.png"
				width:					height
				height:					resizeButton.height
				onClicked:				popupResizeData.close()
				toolTip:				qsTr("Close without resizing data")
				KeyNavigation.tab:		cols

				anchors
				{
					right:				parent.right
					bottom:				inputs.bottom
					margins:			jaspTheme.generalAnchorMargin
				}
			}
		}
	}
}
