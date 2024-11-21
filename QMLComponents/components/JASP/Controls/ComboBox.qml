import QtQuick
import QtQuick.Controls as QTC
import QtQuick.Layouts
import JASP


ComboBoxBase
{
	id:					comboBox
	implicitHeight:		control.height + ((controlLabel.visible && setLabelAbove) ? rectangleLabel.height : 0)
	implicitWidth:		setLabelAbove ? Math.max(control.width, rectangleLabel.width) : (rectangleLabel.width + jaspTheme.labelSpacing + control.width)
	background:			useExternalBorder ? externalControlBackground : control.background
	innerControl:		control
	title:				label

	property alias	control:				control
	property alias	controlLabel:			controlLabel
	property alias	label:					controlLabel.text
	property alias	currentLabel:			comboBox.currentText
	property alias	value:					comboBox.currentValue
	property alias	indexDefaultValue:		comboBox.currentIndex
	property alias	fieldWidth:				control.width
	property int	textFormat:				Text.AutoText
	property bool	showVariableTypeIcon:	containsVariables
	property var	enabledOptions:			[]
	property bool	setLabelAbove:			false
	property int	controlMinWidth:		0
	property bool	useExternalBorder:		true
	property bool	showBorder:				true
	property bool	showEmptyValueAsNormal:	false
	property bool	addLineAfterEmptyValue:	false
	property double controlXOffset:			0
	property bool	alignInGroup:			!setLabelAbove

	onControlMinWidthChanged: _resetWidth(textMetrics.width)
	
	

	function resetWidth(values)
	{
		var maxWidth = 0
		var maxValue = ""
		textMetrics.initialized = false;

		if (addEmptyValue)
			values.push(placeholderText)

		for (var i = 0; i < values.length; i++)
		{
			textMetrics.text = values[i]
			if (textMetrics.width > maxWidth)
			{
				maxWidth = textMetrics.width
				maxValue = values[i]
			}
		}

		textMetrics.text = maxValue;
		textMetrics.initialized = true;
		_resetWidth(maxWidth)
	}

	function _resetWidth(maxTextWidth)
	{
		control.maxTextWidth = maxTextWidth
		// The real field width is composed by the type icon (if displayed) + 2-padding + max width + 5-padding + dropdownIcon width + 2-padding
		var newFieldWidth = (comboBox.showVariableTypeIcon ? contentIcon.x + contentIcon.width : 0) + (allowedTypeIcons.count > 0 ? allowedTypeIcons.width + jaspTheme.itemPadding : 0) + maxTextWidth + dropdownIcon.width + 9 * preferencesModel.uiScale
		if (newFieldWidth < controlMinWidth)
			newFieldWidth = controlMinWidth

		control.realFieldWidth = newFieldWidth
		if (!fixedWidth) control.width = newFieldWidth;
    }

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
						model:					comboBox.model
						anchors
						{
							top:				rectangleLabel.visible && comboBox.setLabelAbove ? rectangleLabel.bottom: comboBox.top
							left:				!rectangleLabel.visible || comboBox.setLabelAbove ? comboBox.left : rectangleLabel.right
							leftMargin:			controlXOffset + (!rectangleLabel.visible || comboBox.setLabelAbove ? 0 : jaspTheme.labelSpacing)
						}

						focus:					true
						padding:				2 * preferencesModel.uiScale
						width:					0
						height:					jaspTheme.comboBoxHeight
						font:					jaspTheme.font
		property bool	isEmptyValue:			comboBox.addEmptyValue && comboBox.currentIndex === 0
		property bool	showEmptyValueStyle:	!comboBox.showEmptyValueAsNormal && isEmptyValue
		property double realFieldWidth:			width
		property double maxTextWidth:			0

		TextMetrics
		{
			id: textMetrics
			font: control.font

			property bool initialized: false

			onWidthChanged:
			{
				if (initialized)
					_resetWidth(width)
			}
		}

		contentItem: Rectangle
		{
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
				anchors.left:				contentIcon.visible ? contentIcon.right : parent.left
				anchors.leftMargin:			2 * preferencesModel.uiScale
				anchors.verticalCenter:		parent.verticalCenter
				anchors.horizontalCenter:	control.showEmptyValueStyle ? parent.horizontalCenter : undefined
				text:						comboBox.currentText
				font:						control.font
				color:						(!enabled || control.showEmptyValueStyle) ? jaspTheme.grayDarker : jaspTheme.black
				width:						(fixedWidth ? widthWhenContralHasFixedWidth : control.maxTextWidth) + 5 * preferencesModel.uiScale
				elide:						Text.ElideRight

				property double widthWhenContralHasFixedWidth: control.width - (x + dropdownIcon.width + 4 * preferencesModel.uiScale) // 4 = leftMargin + 2 padding right of dropdownIcon)

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
			radius:			2
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
			radius:				jaspTheme.jaspControlHighlightWidth
		}

		popup: QTC.Popup
		{
			id:				popupRoot
			y:				control.height
			width:			Math.max(control.realFieldWidth, fieldWidth) + scrollBar.width

			property real	maxHeight: typeof mainWindowRoot !== 'undefined' ? mainWindowRoot.height // Case Dropdowns used in Desktop
																			 : (typeof rcmdRoot !== 'undefined' ? rcmdRoot.height // Case Dropdown used in R Command
																												: (typeof backgroundForms !== 'undefined' ? backgroundForms.height // Case Dropdowns used in Analysis forms
																																						  : Infinity))
			height:			Math.min(popupView.contentHeight + (padding*2), maxHeight)
			padding:		1

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


			contentItem: ListView
			{
				id:				popupView
				width:			popupRoot.width - scrollBar.width
				height:			popupRoot.height
				model:			control.popup.visible ? control.delegateModel : null
				currentIndex:	control.highlightedIndex
				clip:			true

				Rectangle
				{
					anchors.centerIn:	parent
					width:				parent.width + 4
					height:				parent.height + 4
					border.color:		jaspTheme.focusBorderColor
					border.width:		2
					color:				"transparent"
				}
			}
			
			background: Rectangle
			{
				border.color:			jaspTheme.borderColor
				border.width:			1
				color:					jaspTheme.white
			}
		}

		delegate: QTC.ItemDelegate
		{
			height:									jaspTheme.comboBoxHeight
			width:									popupView.width
			enabled:								comboBox.enabledOptions.length == 0 || comboBox.enabledOptions.length <= index || comboBox.enabledOptions[index]

			contentItem: Rectangle
			{
				id:									itemRectangle
				anchors.fill:						parent
				color:								comboBox.currentIndex === index ? jaspTheme.itemSelectedColor : (control.highlightedIndex === index ? jaspTheme.itemHoverColor : jaspTheme.controlBackgroundColor)

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
			}
		}
    }
}
