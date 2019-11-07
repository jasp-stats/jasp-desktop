import QtQuick 2.11
import QtQuick.Controls 2.4
import QtQuick.Layouts 1.3



JASPControl
{
	id:					comboBox
	controlType:		"ComboBox"
	implicitHeight:		control.height + ((controlLabel.visible && setLabelAbove) ? rectangleLabel.height : 0)
	implicitWidth:		control.width + ((controlLabel.visible && !setLabelAbove) ? jaspTheme.labelSpacing + controlLabel.width : 0)
	width:				implicitWidth
	background:			useExternalBorder ? externalControlBackground : control.background


	property alias	control:				control
	property alias	controlLabel:			controlLabel
	property alias	label:					controlLabel.text
	property string	currentText				//Am i empty or what?
	property string	currentColumnType		//Same here
	property alias	currentIndex:			control.currentIndex
	property alias	indexDefaultValue:		control.currentIndex
	property alias	model:					control.model
	property alias	values:					control.model
	property alias	fieldWidth:				control.modelWidth
	property string	textRole:				"label"
	property string	valueRole:				"value"
	property bool	showVariableTypeIcon:	false
	property var	source					//defaults would be nice
	property alias	syncModels:				comboBox.source
	property bool	addEmptyValue:			false
	property string	placeholderText:		qsTr("<no choice>")
	property bool	isDirectModel:			false
	property bool	initialized:			isDirectModel
	property var	enabledOptions:			[]
	property bool	setLabelAbove:			false
	property int	controlMinWidth:		0
	property bool	setWidthInForm:			true
	property bool	useExternalBorder:		true
	property bool	showBorder:				true
	property bool	useModelDefinedIcon:	false

    signal activated(int index);

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

	function _resetWidth(maxWidth)
	{
		var newWidth = maxWidth + ((comboBox.showVariableTypeIcon ? 20 : 4) * preferencesModel.uiScale);
		control.modelWidth = newWidth;
		if (control.width < controlMinWidth)
			control.modelWidth += (controlMinWidth - control.width);
		comboBox.width = comboBox.implicitWidth; // the width is not automatically updated by the implicitWidth...
    }

	Component.onCompleted: control.activated.connect(activated);

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
		}
	}

	ComboBox
	{
						id:				control
						anchors.left:	!rectangleLabel.visible || comboBox.setLabelAbove ? comboBox.left : rectangleLabel.right
						anchors.leftMargin: !rectangleLabel.visible || comboBox.setLabelAbove ? 0 : jaspTheme.labelSpacing
						anchors.top:	rectangleLabel.visible && comboBox.setLabelAbove ? rectangleLabel.bottom: comboBox.top

						focus:			true

						padding:		2 * preferencesModel.uiScale //jaspTheme.jaspControlPadding

						width:			modelWidth + extraWidth
						height:			jaspTheme.comboBoxHeight
						textRole:		comboBox.textRole
		property int	modelWidth:		30 * preferencesModel.uiScale
		property int	extraWidth:		5 * padding + canvas.width
		property bool	isEmptyValue:	comboBox.addEmptyValue && currentIndex <= 0
						font:			jaspTheme.font

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

		contentItem: Item
		{
			height:						jaspTheme.comboBoxHeight
			Image
			{
				id:						contentIcon
				height:					15 * preferencesModel.uiScale
				width:					15 * preferencesModel.uiScale
				x:						3  * preferencesModel.uiScale
				anchors.verticalCenter: parent.verticalCenter
				source:					!visible ? "" : jaspTheme.iconPath + ( enabled ? iconFiles[comboBox.currentColumnType] : iconDisabledFiles[comboBox.currentColumnType] )
				visible:				comboBox.showVariableTypeIcon && comboBox.currentColumnType && !control.isEmptyValue
			}

			Text
			{
				x:							(contentIcon.visible ? 23 : 4) * preferencesModel.uiScale
				text:						control.isEmptyValue ? comboBox.placeholderText : (comboBox.isDirectModel ? control.currentText : comboBox.currentText)
				font:						control.font
				anchors.verticalCenter:		parent.verticalCenter
				anchors.horizontalCenter:	control.isEmptyValue ? parent.horizontalCenter : undefined
				color:						(!enabled || control.isEmptyValue) ? jaspTheme.grayDarker : jaspTheme.black
			}
		}

		indicator: Canvas
		{
			id:				canvas
			x:				control.width - width - 3 //control.spacing
			y:				control.topPadding + (control.availableHeight - height) / 2
			width:			12 * preferencesModel.uiScale
			height:			8  * preferencesModel.uiScale
			contextType:	"2d"

			Connections {
				target:				control
				onPressedChanged:	canvas.requestPaint()
			}

			onPaint: //Is this really the best way to do whatever it is that is being done here? Maybe we can make a custom QuickItem.
			{
				context.reset();
				context.moveTo(0, 0);
				context.lineTo(width, 0);
				context.lineTo(width / 2, height);
				context.closePath();
				context.fillStyle = jaspTheme.grayDarker;
				context.fill();
			}
		}

		background: Rectangle
		{
			id:				comboBoxBackground
			border.width:	comboBox.showBorder && !control.activeFocus ? 1					: 0
			border.color:	comboBox.showBorder							? jaspTheme.borderColor : "transparent"
			radius:			2
			color:			enabled ? jaspTheme.controlBackgroundColor : jaspTheme.disableControlBackgroundColor
		}

		Rectangle
		{
			id:					externalControlBackground
			height:				parent.height + jaspTheme.jaspControlHighlightWidth
			width:				parent.width + jaspTheme.jaspControlHighlightWidth
			color:				"transparent"
			border.width:		3
			border.color:		"transparent"
			anchors.centerIn:	parent
			opacity:			debug ? .3 : 1
			visible:			comboBox.useExternalBorder
			radius:				jaspTheme.jaspControlHighlightWidth
		}

		popup: Popup
		{
			y:				control.height - 1
			width:			comboBoxBackground.width
			padding:		1

			enter: Transition { NumberAnimation { property: "opacity"; from: 0.0; to: 1.0 } }
			contentItem: ListView
			{
				id: popupView
				width:			comboBoxBackground.width
				implicitHeight: contentHeight
				model:			control.popup.visible ? control.delegateModel : null
				currentIndex:	control.highlightedIndex

				Rectangle
				{
					anchors.centerIn: parent
					width: parent.width + 4
					height: parent.height + 4
					border.color:	jaspTheme.focusBorderColor
					border.width:	2
					color: "transparent"
				}
			}
		}

		delegate: ItemDelegate
		{
			height:								jaspTheme.comboBoxHeight
			width:								comboBoxBackground.width
			enabled:							comboBox.enabledOptions.length == 0 || comboBox.enabledOptions.length <= index || comboBox.enabledOptions[index]

			contentItem: Rectangle
			{
				id:								itemRectangle
				anchors.fill:					parent
				color:							comboBox.currentIndex === index ? jaspTheme.itemSelectedColor : (control.highlightedIndex === index ? jaspTheme.itemHoverColor : jaspTheme.controlBackgroundColor)

				property bool isEmptyValue:		comboBox.addEmptyValue && index <= 0

				Image
				{
					id:							delegateIcon
					x:							1 * preferencesModel.uiScale
					height:						15 * preferencesModel.uiScale
					width:						15 * preferencesModel.uiScale
					source:						useModelDefinedIcon ? model.iconfile : ((visible && comboBox.initialized) ? jaspTheme.iconPath + (enabled ? iconFiles[model.columnType] : iconDisabledFiles[model.columnType]) : "")
					visible:					comboBox.showVariableTypeIcon && !itemRectangle.isEmptyValue

					anchors.verticalCenter:		parent.verticalCenter
				}

				Text {
					x:							(delegateIcon.visible ? 20 : 4) * preferencesModel.uiScale
					text:						comboBox.initialized ? (itemRectangle.isEmptyValue ? comboBox.placeholderText : (comboBox.isDirectModel ? model.label : model.name)) : ""
					font:						jaspTheme.font
					color:						itemRectangle.isEmptyValue || !enabled ? jaspTheme.grayDarker : (comboBox.currentIndex === index ? jaspTheme.white : jaspTheme.black)
					anchors.verticalCenter:		parent.verticalCenter
					anchors.horizontalCenter:	itemRectangle.isEmptyValue ? parent.horizontalCenter : undefined
				}
			}
		}
    }
}
