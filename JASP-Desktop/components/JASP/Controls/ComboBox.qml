import QtQuick 2.11
import QtQuick.Controls 2.4
import QtQuick.Layouts 1.3
import JASP.Theme 1.0

JASPControl {
	id:					comboBox
	controlType:		"ComboBox"
	implicitHeight:		control.height + ((controlLabel.visible && setLabelAbove) ? rectangleLabel.implicitHeight : 0)
	implicitWidth:		control.implicitWidth + ((controlLabel.visible && !setLabelAbove) ? labelSpacing + controlLabel.implicitWidth : 0)
	width:				implicitWidth
    
	property alias	control:				control
	property int	labelSpacing:			4 * preferencesModel.uiScale
	property alias	controlLabel:			controlLabel
	property alias	label:					controlLabel.text
	property string	currentText				//Am i empty or what?
	property string	currentColumnType		//Same here
	property alias	currentIndex:			control.currentIndex
	property alias	indexDefaultValue:		control.currentIndex
	property alias	model:					control.model
	property alias	values:					control.model
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
		if (control.implicitWidth < controlMinWidth)
			control.modelWidth += (controlMinWidth - control.implicitWidth);
		comboBox.width = comboBox.implicitWidth; // the width is not automatically updated by the implicitWidth...
    }
    
	Component.onCompleted: control.activated.connect(activated);
    
	Rectangle
	{
		id: rectangleLabel
		implicitWidth: controlLabel.implicitWidth
		implicitHeight: control.implicitHeight
		color: debug ? Theme.debugBackgroundColor : "transparent"
		visible: controlLabel.text && comboBox.visible ? true : false
		Label
		{
			id:			controlLabel
			font:		Theme.font
			anchors.verticalCenter: parent.verticalCenter
			color:		enabled ? Theme.textEnabled : Theme.textDisabled
		}
	}

	ComboBox
	{
						id:				control
						anchors.left:	!rectangleLabel.visible || comboBox.setLabelAbove ? comboBox.left : rectangleLabel.right
						anchors.leftMargin: !rectangleLabel.visible || comboBox.setLabelAbove ? 0 : comboBox.labelSpacing
						anchors.top:	rectangleLabel.visible && comboBox.setLabelAbove ? rectangleLabel.bottom: comboBox.top

						focus:			true

						padding:		2 * preferencesModel.uiScale //Theme.jaspControlPadding

						implicitWidth:	modelWidth + extraWidth
						textRole:		comboBox.textRole
		property int	modelWidth:		30 * preferencesModel.uiScale
		property int	extraWidth:		5 * padding + canvas.width
		property bool	isEmptyValue:	comboBox.addEmptyValue && currentIndex <= 0
						font:			Theme.font

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
			implicitHeight:				Theme.comboBoxHeight
			Image
			{
				id:						contentIcon
				height:					15 * preferencesModel.uiScale
				width:					15 * preferencesModel.uiScale
				x:						3  * preferencesModel.uiScale
				anchors.verticalCenter: parent.verticalCenter
				source:					!visible ? "" : enabled ? iconFiles[comboBox.currentColumnType] : iconDisabledFiles[comboBox.currentColumnType]
				visible:				comboBox.showVariableTypeIcon && comboBox.currentColumnType && !control.isEmptyValue
			}

			Text
			{
				x:							(contentIcon.visible ? 23 : 4) * preferencesModel.uiScale
				text:						control.isEmptyValue ? comboBox.placeholderText : (comboBox.isDirectModel ? control.currentText : comboBox.currentText)
				font:						control.font
				anchors.verticalCenter:		parent.verticalCenter
				anchors.horizontalCenter:	control.isEmptyValue ? parent.horizontalCenter : undefined
				color:						(!enabled || control.isEmptyValue) ? Theme.grayDarker : Theme.black
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
				context.fillStyle = Theme.grayDarker;
				context.fill();
			}
		}

		background: Rectangle
		{
			id:				comboBoxBackground
			border.color:	Theme.borderColor
			border.width:	1
			radius:			2
			color:			enabled ? Theme.controlBackgroundColor : Theme.disableControlBackgroundColor
		}

		popup: Popup
		{
			y:				control.height - 1
			width:			control.width
			implicitHeight:	contentItem.implicitHeight
			padding:		1

			enter: Transition { NumberAnimation { property: "opacity"; from: 0.0; to: 1.0 } }
			contentItem: ListView
			{
				id: popupView
				clip:			true
				implicitHeight:	contentHeight
				model:			control.popup.visible ? control.delegateModel : null
				currentIndex:	control.highlightedIndex
			}

			background: Rectangle
			{
				border.color:	Theme.borderColor
				radius:			2
			}
		}

		delegate: ItemDelegate
		{
			height:								Theme.comboBoxHeight
			highlighted:						control.highlightedIndex === index
			enabled:							comboBox.enabledOptions.length == 0 || comboBox.enabledOptions.length <= index || comboBox.enabledOptions[index]

			contentItem: Rectangle
			{
				id:								itemRectangle
				anchors.fill:					parent

				property bool isEmptyValue:		comboBox.addEmptyValue && index <= 0

				Image
				{
					id:							delegateIcon
					x:							1 * preferencesModel.uiScale
					height:						15 * preferencesModel.uiScale
					width:						15 * preferencesModel.uiScale
					source:						(visible && comboBox.initialized) ? (enabled ? iconFiles[model.columnType] : iconDisabledFiles[model.columnType]) : ""
					visible:					comboBox.showVariableTypeIcon && !itemRectangle.isEmptyValue

					anchors.verticalCenter:		parent.verticalCenter
				}

				Text {
					x:							(delegateIcon.visible ? 20 : 4) * preferencesModel.uiScale
					text:						comboBox.initialized ? (itemRectangle.isEmptyValue ? comboBox.placeholderText : (comboBox.isDirectModel ? model.label : model.name)) : ""
					font:						Theme.font
					color:						itemRectangle.isEmptyValue || !enabled ? Theme.grayDarker : Theme.black
					verticalAlignment:			Text.AlignVCenter
					anchors.horizontalCenter:	itemRectangle.isEmptyValue ? parent.horizontalCenter : undefined
				}
			}
		}
    }
}
