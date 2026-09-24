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
				anchors.top:	parent.top
				anchors.left:	parent.left
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
						KeyNavigation.tab:		importLanguage
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
		}
	}

	// Which locale the numbers in this file are written in. It starts out as the language JASP itself is set to
	// but only applies to this import, the preferences are left alone. See CsvPreviewModel.
	Rectangle
	{
		id:					importLocaleRect
		anchors.top:		delimetersRect.bottom
		anchors.left:		parent.left
		anchors.right:		parent.right
		anchors.margins:	windowPadding
		height:				localeColumn.implicitHeight + 2 * jaspTheme.generalAnchorMargin
		color:				jaspTheme.uiBackground

		// The whole range holds names like "South Georgia & South Sandwich Islands", wider than this entire window,
		// so the closed field is capped (it elides) while the popup stays as wide as it needs to be.
		property real fieldWidth: 150 * jaspTheme.uiScale

		Column
		{
			id:					localeColumn
			anchors.top:		parent.top
			anchors.left:		parent.left
			anchors.right:		parent.right
			anchors.margins:	jaspTheme.generalAnchorMargin
			spacing:			jaspTheme.rowSpacing

			Row
			{
				id:			localeRow
				spacing:	jaspTheme.rowSpacing

				JC.DropDown
				{
					id:						importLanguage
					label:					qsTr("Read numbers as written in")
					values:					csvPreviewModel.languages
					startValue:				csvPreviewModel.language
					value:					csvPreviewModel.language
					addEmptyValue:			false
					control.width:			Math.min(control.implicitWidth, importLocaleRect.fieldWidth)
					onValueChanged:			if(value !== "") csvPreviewModel.language = value
					KeyNavigation.priority:	KeyNavigation.BeforeItem
					KeyNavigation.tab:		moreLanguages
					KeyNavigation.backtab:	delimiterRepeater.itemAt(0)
				}

				JC.CheckBox
				{
					id:						moreLanguages
					anchors.verticalCenter:	importLanguage.verticalCenter
					label:					qsTr("More languages")
					toolTip:				qsTr("Offer every language instead of the ones JASP is translated into, and let the territory be chosen as well.")
					onCheckedChanged:		csvPreviewModel.moreLanguages = checked
					KeyNavigation.priority:	KeyNavigation.BeforeItem
					KeyNavigation.tab:		checked ? importTerritory : submitButton
					KeyNavigation.backtab:	importLanguage

					// The model ticks it as well, when the locale of the interface can only be shown by the complete list
					Binding on checked { value: csvPreviewModel.moreLanguages }
				}
			}

			JC.DropDown
			{
				id:						importTerritory
				label:					qsTr("Territory")
				values:					csvPreviewModel.territories
				startValue:				csvPreviewModel.territory
				value:					csvPreviewModel.territory
				addEmptyValue:			false
				visible:				moreLanguages.checked
				// Widen the label so this field lines up right under the language field instead of under its label
				controlLabel.width:		Math.max(importTerritory.controlLabel.implicitWidth, importLanguage.controlLabel.width)
				control.width:			Math.min(control.implicitWidth, importLocaleRect.fieldWidth)
				onValueChanged:			if(value !== "") csvPreviewModel.territory = value
				KeyNavigation.priority:	KeyNavigation.BeforeItem
				KeyNavigation.tab:		submitButton
				KeyNavigation.backtab:	moreLanguages
			}

			// What that locale does to a number, so the user can see before pressing Load that 86.298 is not always 86.298
			Rectangle
			{
				implicitWidth:		parseExampleText.implicitWidth
				implicitHeight:		parseExampleText.implicitHeight
				width:				implicitWidth
				height:				implicitHeight

				color:				jaspTheme.white
				border.width:		1
				border.color:		jaspTheme.borderColor
				radius:				jaspTheme.borderRadius

				Text
				{
					id:				parseExampleText

					text:			csvPreviewModel.parseExample
					color:			jaspTheme.textEnabled
					font:			jaspTheme.font
					padding:		jaspTheme.itemPadding
				}
			}
		}
	}

	// Data Preview
	Rectangle
	{
		anchors
		{
			left:				parent.left
			right:				parent.right
			top:				importLocaleRect.bottom
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
			KeyNavigation.backtab:	moreLanguages.checked ? importTerritory : moreLanguages
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
