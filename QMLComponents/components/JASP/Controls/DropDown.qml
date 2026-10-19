//
// Copyright (C) 2013-2026 University of Amsterdam
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
//
// Copyright (C) 2013-2026 University of Amsterdam
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
import QtQuick.Controls as QTC
import QtQuick.Layouts
import JASP.Controls

/*!
    \qmltype DropDown
    \inqmlmodule JASP.Controls 1.0
    \brief A selection control that presents a list of options in a dropdown menu.

    Returns the value of the selected item as a string to R.

    \section1 R Binding

    \list
    \li \b{R Type:} \c character
    \li \b{Default:} First item's value, or "" if addEmptyValue is true
    \endlist

    \section1 Properties

    \list
    \li \b name (string) - R option name this control binds to. Default: "".
    \li \b label (string) - Label displayed before the dropdown. Default: "".
    \li \b currentValue (var) - The value of the currently selected item.
    \li \b currentIndex (int) - Index of currently selected item. Default: 0.
    \li \b values (array) - Simple array of values (creates value=label pairs). Default: [].
    \li \b source (var) - Source for populating from variables or other controls. Can be the id or the name (or an array of ids/names) of the controls.
    \li \b addEmptyValue (bool) - Add an empty option at the start. Default: false.
    \li \b placeholderText (string) - Text shown when empty value is selected. Default: "<no choice>".
    \endlist

    \section1 Inherited Properties

    \list
    \li \b enabled (bool) - Whether the control is interactive. Default: true.
    \li \b visible (bool) - Whether the control is visible. Default: true.
    \li \b info (string) - Info that will be used by tooltip and to generate the help. Default: "".
    \li \b toolTip (string) - This property overwrite info property, in order to display a simpler tooltip text. Default: "".
    \endlist

    \section1 Signals

    \list
    \li \b activated(int index) - Emitted when user selects an item.
    \endlist

    \section1 Example

    \qml
    Column {
        DropDown {
            name: "correlationType"
            label: qsTr("Correlation coefficient")
            values: [
                { label: qsTr("Pearson"),  value: "pearson"  },
                { label: qsTr("Spearman"), value: "spearman" }
            ]
            indexDefaultValue: 0
        }
        DropDown {
            name: "factor"
            label: qsTr("Choose factor variable")
            source: factors // id of the factors VariablesList
        }
    }

    \endqml
*/
ComboBoxBase
{
	id:					comboBox
	height:				implicitHeight
	width:				implicitWidth
	implicitHeight:		control.height + ((controlLabel.visible && setLabelAbove) ? rectangleLabel.height : 0)
	implicitWidth:		controlLabel.visible
							? (setLabelAbove ? Math.max(control.width, rectangleLabel.width) : (rectangleLabel.width + jaspTheme.labelSpacing + control.width))
							: control.width
	background:			useExternalBorder ? externalControlBackground : control.background
	innerControl:		control
	title:				label

	property alias	control:				control
	property alias	controlLabel:			controlLabel
	property alias	label:					controlLabel.text
	property alias	currentLabel:			comboBox.currentText
	property alias	value:					comboBox.currentValue
	property alias	indexDefaultValue:		comboBox.currentIndex
	property alias	fieldWidth:				control.implicitWidth
	property int	textFormat:				Text.AutoText
	property bool	showVariableTypeIcon:	containsVariables
	property var	enabledOptions:			[]
	property bool	setLabelAbove:			false
	property bool	useExternalBorder:		true
	property bool	showBorder:				true
	property bool	showEmptyValueAsNormal:	false
	property bool	addLineAfterEmptyValue:	false
	property double controlXOffset:			0
	property bool	alignInGroup:			!setLabelAbove

	Component.onCompleted:	control.activated.connect(activated);

	Rectangle
	{
		id:			rectangleLabel
		width:		controlLabel.width
		height:		control.height
		color:		debug ? jaspTheme.debugBackgroundColor : "transparent"
		visible:	controlLabel.text && comboBox.visible ? true : false
		Label
		{
			id:			controlLabel
			font:		jaspTheme.font
			anchors.verticalCenter: parent.verticalCenter
			color:		enabled ? jaspTheme.textEnabled : jaspTheme.textDisabled
			width:		implicitWidth
			textFormat: comboBox.textFormat
		}
	}

	QTC.ComboBox
	{
						id:						control
						implicitWidth:			longestFieldWidth + (allowedTypeIcons.count > 0 ? allowedTypeIcons.width + jaspTheme.contentMargin : 0)
						model:					comboBox.model
						anchors
						{
							top:				rectangleLabel.visible && comboBox.setLabelAbove ? rectangleLabel.bottom: comboBox.top
							left:				!rectangleLabel.visible || comboBox.setLabelAbove ? comboBox.left : rectangleLabel.right
							leftMargin:			controlXOffset + (!rectangleLabel.visible || comboBox.setLabelAbove ? 0 : jaspTheme.labelSpacing)
						}

						focus:					true
						padding:				2 * preferencesModel.uiScale
						height:					jaspTheme.comboBoxHeight
						font:					jaspTheme.font
		property bool	isEmptyValue:			comboBox.addEmptyValue && comboBox.currentIndex === 0
		property bool	showEmptyValueStyle:	!comboBox.showEmptyValueAsNormal && isEmptyValue
		property double	longestFieldWidth:		(comboBox.showVariableTypeIcon ? contentIcon.x + contentIcon.width + jaspTheme.contentMargin : 0) +
												textMetrics.width + indicator.width + 3 * jaspTheme.contentMargin

		TextMetrics
		{
			id: textMetrics
			font: control.font
			text: longestValue
		}

		contentItem: Rectangle
		{
			id: contentRectangle
			color:	jaspTheme.controlBackgroundColor

			Image
			{
				id:						contentIcon
				height:					15 * preferencesModel.uiScale
				width:					15 * preferencesModel.uiScale // Even if not visible, the width should stay the same: if showVariableTypeIcon is true, a value may have no icon, but an empty icon place should still be displayed
				x:						2  * preferencesModel.uiScale
				anchors.verticalCenter: parent.verticalCenter
				source:					!visible ? "" : ((comboBox.currentColumnTypeIcon && comboBox.isBound) ? comboBox.currentColumnTypeIcon : (comboBox.values && comboBox.currentIndex >= 0 && comboBox.currentIndex < comboBox.values.length ? comboBox.values[comboBox.currentIndex].columnTypeIcon : ""))
				visible:				comboBox.showVariableTypeIcon && !control.isEmptyValue && (comboBox.currentColumnType || !comboBox.isBound)
			}

			Text
			{
				id:							controlText
				anchors
				{
					left:					contentIcon.visible ? contentIcon.right : parent.left
					leftMargin:				2 * preferencesModel.uiScale
					right:					allowedColumnsIcons.length > 0 ? allowedTypeIcons.left : parent.right
					verticalCenter:			parent.verticalCenter
					//horizontalCenter:		control.showEmptyValueStyle ? parent.horizontalCenter : undefined
				}
				text:						comboBox.currentText
				font:						control.font
				color:						(!enabled || control.showEmptyValueStyle) ? jaspTheme.grayDarker : jaspTheme.black
				elide:						Text.ElideRight
			}

			AllowedTypeIcons
			{
				id:			allowedTypeIcons
				iconModel:	allowedColumnsIcons

				anchors
				{
					bottomMargin:	jaspTheme.contentMargin
					right:			parent.right
					rightMargin:	jaspTheme.contentMargin
				}
			}
		}

		indicator: Image
		{
			id:			dropdownIcon
			x:			control.width - width - 2 * preferencesModel.uiScale
			y:			control.topPadding + (control.availableHeight - height) / 2
			width:		12 * preferencesModel.uiScale
			height:		12 * preferencesModel.uiScale
			source:		jaspTheme.iconPath + "/toolbutton-menu-indicator.svg"

		}

		background: Rectangle
		{
			id:				comboBoxBackground
			border.width:	comboBox.showBorder && !control.activeFocus ? 1					: 0
			border.color:	comboBox.showBorder							? jaspTheme.borderColor : "transparent"
			radius:			jaspTheme.borderRadius
			color:			jaspTheme.controlBackgroundColor
		}

		Rectangle
		{
			id:					externalControlBackground
			height:				parent.height + jaspTheme.jaspControlHighlightWidth
			width:				parent.width + jaspTheme.jaspControlHighlightWidth
			color:				"transparent"
			border.width:		1
			border.color:		"transparent"
			anchors.centerIn:	parent
			opacity:			debug ? .3 : 1
			visible:			comboBox.useExternalBorder
			radius:				jaspTheme.borderRadius
		}

		popup: QTC.Popup
		{
			id:				popupRoot
			padding:		1
			implicitWidth:	contentWidth + scrollBar.width
			implicitHeight: Math.min(contentHeight + 2 * padding, popupView.maxHeight)
			x:				padding
			y:				padding

			enter: Transition { NumberAnimation { property: "opacity"; from: 0.0; to: 1.0 } enabled: preferencesModel.animationsOn }

			JASPScrollBar
			{
				id:				scrollBar
				flickable:		popupView
				manualAnchor:	true
				vertical:		true
				z:				1337

				anchors
				{
					top:		parent.top
					right:		parent.right
					bottom:		parent.bottom
				}
			}

			contentItem: Item
			{
				id:					popupItem
				implicitWidth:		popupView.implicitWidth
				implicitHeight:		popupView.implicitHeight
				
				ListView
				{
					id:				popupView
					implicitWidth:	Math.max(control.longestFieldWidth, control.width)
					implicitHeight:	Math.min(contentHeight, maxHeight)
					model:			control.delegateModel
					currentIndex:	control.highlightedIndex
					clip:			true
					anchors.fill:	parent
	
					property real	maxHeight: typeof mainWindowRoot	!== 'undefined' ? mainWindowRoot.height		// Case Dropdowns used in Desktop
											 : typeof csvPreviewWindow	!== 'undefined' ? csvPreviewWindow.height	// Case CSV Preview
											 : typeof rcmdRoot			!== 'undefined' ? rcmdRoot.height			// Case Dropdown used in R Command
											 : typeof backgroundForms	!== 'undefined' ? backgroundForms.height	// Case Dropdowns used in Analysis forms
											 : typeof scrollPrefs		!== 'undefined' ? scrollPrefs.height		// When its used in a Prefs* page ?
											 : Infinity
	

					//onMaxHeightChanged:		messages.log("maxHeight is now " + maxHeight + " for " + popupView);
	
					Rectangle
					{
						anchors.centerIn:	parent
						width:				parent.width + 4
						height:				parent.height + 4
						border.color:		jaspTheme.focusBorderColor
						border.width:		2
						color:				"transparent"
						radius:				jaspTheme.borderRadius
					}
				}
				
				ScrollMoreIndicator
				{
					anchors
					{
						top:	popupView.top
						left:	popupView.left
						right:	popupView.right
					}
					
					upsideDown:	true
					extraSpace:	popupView.contentY
				}
				
				ScrollMoreIndicator
				{
					anchors
					{
						left:	popupView.left
						right:	popupView.right
						bottom:	popupView.bottom
					}
					extraSpace:	popupView.contentHeight - (popupView.contentY + popupView.height)
				}
			}
			
			background: Rectangle
			{
				border.color:			jaspTheme.borderColor
				border.width:			1
				color:					jaspTheme.fileMenuColorBackground
				radius:					jaspTheme.borderRadius	
			}
		}

		delegate: QTC.ItemDelegate
		{
			implicitHeight:							jaspTheme.comboBoxHeight
			implicitWidth:							popupView.width
			enabled:								(comboBox.enabledOptions.length == 0 || comboBox.enabledOptions.length <= index || comboBox.enabledOptions[index])

			contentItem: Rectangle
			{
				id:									itemRectangle
				anchors.fill:						parent
				color:								comboBox.currentIndex === index ? jaspTheme.itemSelectedColor : (control.highlightedIndex === index ? jaspTheme.itemHoverColor : "transparent")

				property bool isEmptyValue:			comboBox.addEmptyValue && index === 0
				property bool showEmptyValueStyle:	!comboBox.showEmptyValueAsNormal && isEmptyValue
				property bool showLine:				comboBox.addLineAfterEmptyValue && index === 0

				Image
				{
					id:								delegateIcon
					x:								1 * preferencesModel.uiScale
					height:							15 * preferencesModel.uiScale
					width:							15 * preferencesModel.uiScale
					source:							visible ? (comboBox.isBound ? model.columnTypeIcon : comboBox.values[index].columnTypeIcon) : ""
					visible:						comboBox.showVariableTypeIcon && !itemRectangle.isEmptyValue

					anchors.verticalCenter:			parent.verticalCenter
				}

				Text
				{
					x:								(delegateIcon.visible ? 20 : 4) * preferencesModel.uiScale
					text:							itemRectangle.isEmptyValue ? comboBox.placeholderText : (model && model.name ? model.name : "")
					font:							jaspTheme.font
					color:							itemRectangle.showEmptyValueStyle || !enabled ? jaspTheme.grayDarker : (comboBox.currentIndex === index ? jaspTheme.white : jaspTheme.black)
					anchors.verticalCenter:			parent.verticalCenter
					anchors.horizontalCenter:		itemRectangle.showEmptyValueStyle ? parent.horizontalCenter : undefined
				}

				Rectangle
				{
					anchors
					{
						left:						parent.left
						right:						parent.right
						bottom:						parent.bottom
					}
					visible:						itemRectangle.showLine
					height:							1
					color:							jaspTheme.focusBorderColor
				}

				MouseArea
				{
					anchors.fill:					parent
					acceptedButtons:				Qt.NoButton
					QTC.ToolTip.text:				model.info
					QTC.ToolTip.visible:			model.info !== "" && containsMouse
					cursorShape:					Qt.PointingHandCursor
					hoverEnabled:					true
					z:								10
				}
			}
		}
    }
}
