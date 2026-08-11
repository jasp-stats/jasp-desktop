//
// Copyright (C) 2026 University of Amsterdam
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
import QtQuick.Layouts
import JASP.Controls as JC
import JASP.Widgets

Window
{
	id: csvPreviewWindow

	minimumWidth:           600 * jaspTheme.uiScale
	minimumHeight:          600 * jaspTheme.uiScale
	visible:                csvPreviewModel.visible
	title:					qsTr("Data Preview")
	modality:               Qt.ApplicationModal
	color:                  jaspTheme.white

	Accessible.role:		Accessible.Window
	Accessible.name:		title

	property real windowPadding: 20 * jaspTheme.uiScale

	onVisibleChanged:
	{
		if (!visible) return

		submitButton.forceActiveFocus()
	}

	onClosing:
	{
		csvPreviewModel.delimiter = '\0'
		csvPreviewModel.visible = false
	}

	Shortcut { sequence: "Escape"; onActivated: cancelButton.clicked() }

	// Header / Toolbar for delimiter selection
	Rectangle
	{
		id: delimetersRect
		anchors
		{
			top:				parent.top
			left:				parent.left
			right:				parent.right
			margins:			windowPadding
		}

		height:					delimiterRow.implicitHeight + 2 * jaspTheme.generalAnchorMargin
		color:					jaspTheme.uiBackground

		Item
		{
			id:					delimetersInsideRect
			anchors.fill:		parent
			anchors.margins:	jaspTheme.generalAnchorMargin

			RowLayout
			{
				id:				delimiterRow
				spacing:		jaspTheme.rowSpacing

				JC.Label
				{
					text:		qsTr("Select Delimiter:")
					font.bold:	true
					Layout.alignment: Qt.AlignHCenter
				}

				Repeater
				{
					id:		delimiterRepeater
					model:	[ ',', '.', ';', ':', '|', '\t', ' ' ]

					JC.RoundedButton
					{
						text:				modelData == ' ' ? qsTr("Space") : modelData == '\t' ? qsTr("Tab") : modelData
						onClicked:			csvPreviewModel.delimiter = modelData
						selected:			csvPreviewModel.delimiter == modelData
						color:				selected ? jaspTheme.buttonColorPressed : defaultColor
						Layout.minimumWidth: 30 * jaspTheme.uiScale

						// Treat the delimiter buttons as a radio group: Tab exits the group,
						// Left/Right navigate within it.
						activeFocusOnTab:		false
						KeyNavigation.tab:		advanced
						KeyNavigation.backtab:	cancelButton

						Keys.onLeftPressed: (event) =>
						{
							event.accepted = true
							if (index > 0) delimiterRepeater.itemAt(index - 1).forceActiveFocus()
							else delimiterRepeater.itemAt(delimiterRepeater.count - 1).forceActiveFocus()
						}
						Keys.onRightPressed: (event) =>
						{
							event.accepted = true
							if (index < delimiterRepeater.count - 1) delimiterRepeater.itemAt(index + 1).forceActiveFocus()
							else delimiterRepeater.itemAt(0).forceActiveFocus()
						}
					}
				}
			}

			JC.CheckBox
			{
				id:						advanced
				anchors.right:			parent.right
				anchors.rightMargin:	jaspTheme.generalAnchorMargin
				anchors.verticalCenter: parent.verticalCenter
				label:					qsTr("Advanced")
				KeyNavigation.priority:	KeyNavigation.BeforeItem
				KeyNavigation.tab:		submitButton
				KeyNavigation.backtab:	delimiterRepeater.itemAt(0)
			}
		}
	}

	PrefsLanguage
	{
		id:					prefLanguage
		anchors.top:		delimetersRect.bottom
		anchors.left:		parent.left
		anchors.right:		parent.right
		anchors.margins:	windowPadding
		visible:			advanced.checked
		nextTabItem:		submitButton
		showHelpLink:		false
	}

	// Data Preview
	Rectangle
	{
		anchors
		{
			left:				parent.left
			right:				parent.right
			top:				prefLanguage.visible ? prefLanguage.bottom : delimetersRect.bottom
			bottom:				buttons.top
			margins:			windowPadding
		}
		color:					jaspTheme.white
		border.width:			1
		border.color:			jaspTheme.black

		JC.JASPScrollBar
		{
			id:				vertiScroller;
			flickable:		dataTableView
			anchors.top:	parent.top
			anchors.right:	parent.right
			anchors.bottom: horiScroller.top
		}

		JC.JASPScrollBar
		{
			id:				horiScroller;
			flickable:		dataTableView
			vertical:		false
			anchors.left:	parent.left
			anchors.right:	vertiScroller.left
			anchors.bottom: parent.bottom
		}

		TableView
		{
			id:							dataTableView

			anchors.top:				parent.top
			anchors.left:				parent.left
			anchors.right:				vertiScroller.left
			anchors.bottom:				horiScroller.top
			anchors.margins:			1

			clip:						true

			model:						csvPreviewModel
			reuseItems:					false

			Connections
			{
				target:			csvPreviewModel

				function onClearTableForResize()
				{
					dataTableView.contentX = 0
					dataTableView.contentY = 0
					dataTableView.model = null;
					dataTableView.model = csvPreviewModel;
				}
			}

			delegate:					Rectangle
			{
				implicitHeight:			theText.contentHeight + jaspTheme.generalAnchorMargin
				implicitWidth:			theText.contentWidth  + jaspTheme.generalAnchorMargin

				color:					jaspTheme.white
				border.width:			1
				border.color:			jaspTheme.uiBorder

				Text
				{
					id:					theText
					text:				modelData
					color:				jaspTheme.textEnabled
					font:				jaspTheme.font
					anchors.centerIn:	parent
				}
			}
		}
	}

	Row
	{
		id: buttons
		spacing: 10 * jaspTheme.uiScale

		anchors.bottom:         parent.bottom
		anchors.bottomMargin:   windowPadding
		anchors.left:           parent.left
		anchors.leftMargin:     windowPadding

		property real buttonWidth: (csvPreviewWindow.width - windowPadding * 2 - buttons.spacing) / 2

		JC.Button
		{
			id: submitButton
			text: qsTr("Load")
			width: buttons.buttonWidth

			control.color: activeFocus ? jaspTheme.blueDarker : jaspTheme.blue
			onClicked: csvPreviewModel.visible = false
			KeyNavigation.priority:	KeyNavigation.BeforeItem
			KeyNavigation.tab:		cancelButton
			KeyNavigation.backtab:	advanced
		}

		JC.Button
		{
			id: cancelButton
			text: qsTr("Cancel")
			width: buttons.buttonWidth
			onClicked:
			{
				csvPreviewModel.delimiter = '\0'
				csvPreviewModel.visible = false
			}
			KeyNavigation.priority:	KeyNavigation.BeforeItem
			KeyNavigation.tab:		delimiterRepeater.itemAt(0)
			KeyNavigation.backtab:	submitButton
		}
	}
}
