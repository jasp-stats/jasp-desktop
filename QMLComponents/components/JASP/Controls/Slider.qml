import QtQuick				2.11
import QtQuick.Controls		2.4
import JASP.Controls		1.0 as JC //He returns!
import QtQuick.Layouts		1.3
import JASP					1.0

SliderBase
{
	id:					slider
	implicitHeight:		columnLayout.implicitHeight
	implicitWidth:		columnLayout.implicitWidth
	innerControl:		textField
	title:				controlLabel.text
	focusOnTab:			false

	property alias	control:		textField
	property int	decimals:		2
	property int	power:			Math.pow(10, decimals);
	property alias	label:			controlLabel.text
	property alias	controlLabel:	controlLabel
	property alias	value:			control.value
	property bool	vertical:		true
	property int	verticalInt:	vertical ? Qt.Vertical : Qt.Horizontal
	property alias	orientation:	control.orientation
	property alias	stepSize:		control.stepSize
	property alias	from:			control.from
	property alias	to:				control.to
	property alias	min:			control.from
	property alias	max:			control.to
	property alias	textField:		textField

	Component.onCompleted: control.moved.connect(moved);

	GridLayout
	{
		id:				columnLayout;
		rows:			control.vertical ? 3 : 1
		columns:		control.vertical ? 1 : 3

		Label
		{
			id:			controlLabel
			visible:	controlLabel.text && slider.visible ? true : false
			font:		jaspTheme.font
			color:		enabled ? jaspTheme.textEnabled : jaspTheme.textDisabled
		}

		Slider
		{
			id:					control
			Layout.alignment:	Qt.AlignCenter
			Layout.leftMargin:	control.orientation === Qt.Vertical ? leftPadding + jaspTheme.sliderWidth : 0
			value:				0.5
			stepSize:			1 / slider.power
			orientation:		slider.verticalInt
			activeFocusOnTab:	false

			background: Rectangle
			{
				id:				sliderBackground
				x:				control.leftPadding
				y:				control.topPadding + (control.vertical ? 0 : control.height / 4)
				implicitWidth:	control.vertical ? jaspTheme.sliderWidth : jaspTheme.sliderLength
				implicitHeight: control.vertical ? jaspTheme.sliderLength : jaspTheme.sliderWidth
				width:			control.vertical ? implicitWidth : control.availableWidth
				height:			control.vertical ? control.availableHeight : implicitHeight
				radius:			jaspTheme.sliderWidth / 2
				color:			control.vertical ? jaspTheme.sliderPartOn : jaspTheme.sliderPartOff

				Rectangle
				{
					width:		control.vertical ? parent.width : control.visualPosition * parent.width
					height:		control.vertical ? control.visualPosition * parent.height : parent.height
					color:		control.vertical ? jaspTheme.sliderPartOff : jaspTheme.sliderPartOn
					radius:		jaspTheme.sliderWidth / 2
				}
			}

			handle: Rectangle
			{
				id:				sliderHandle
				x:				control.leftPadding + (control.vertical ? sliderBackground.radius - sliderHandle.radius : control.visualPosition * (control.availableWidth - width))
				y:				control.topPadding + (control.vertical ? control.visualPosition * (control.availableHeight - height) : sliderBackground.radius - sliderHandle.radius) + (control.vertical ? 0 : control.height / 4)
				implicitWidth:	jaspTheme.sliderHandleDiameter
				implicitHeight: jaspTheme.sliderHandleDiameter
				radius:			jaspTheme.sliderHandleDiameter / 2
				color:			control.pressed ? jaspTheme.itemSelectedColor : jaspTheme.controlBackgroundColor
				border.color:	jaspTheme.borderColor
			}

			onMoved:
			{
				var intVal  = Math.round(value * slider.power);
				var realVal = intVal / slider.power;

				if (textField.value != realVal) textField.value = realVal;
			}

			onValueChanged:
			{
				var intVal = Math.round(value * slider.power);
				var realVal = intVal / slider.power;
				value = realVal;
			}

		}

		JC.DoubleField
		{
			id:					textField
			value:				control.value
			isBound:			false
			Layout.alignment:	Qt.AlignCenter
			validator:			JASPDoubleValidator { bottom: control.from; top: control.to; decimals: slider.decimals }

			onEditingFinished:
			{
				if (control.value != displayValue)
				{
					control.value = displayValue;
					control.moved();
				}
			}
			onTextEdited:
			{
				if (value && !textField.innerControl.acceptableInput)
					value = control.value
			}
		}
	}
}
