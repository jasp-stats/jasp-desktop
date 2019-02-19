import QtQuick				2.11
import QtQuick.Controls		2.4
import JASP.Controls		1.0 as JC //He returns!
import QtQuick.Layouts		1.3
import JASP.Theme			1.0

JASPControl
{
	id:					slider
	controlType:		"Slider"
	implicitHeight:		control.height + (controlLabel.visible ? labelSpacing + controlLabel.implicitWidth : 0)
	implicitWidth:		control.width
    
	property alias	control:		textField
	property int	labelSpacing:	4 * preferencesModel.uiScale
	property int	decimals:		2
	property int	power:			Math.pow(10, decimals);
	property alias	label:			controlLabel.text
	property alias	title:			controlLabel.text
	property alias	controlLabel:	controlLabel
	property alias	value:			control.value
	property alias	orientation:	control.orientation
	property alias	stepSize:		control.stepSize
	property alias	from:			control.from
	property alias	to:				control.to
	property alias	textField:		textField
    
    signal moved();
    
	Component.onCompleted: control.moved.connect(moved);
    
	ColumnLayout
	{
		id:			columnLayout;
		spacing:	controlLabel.visible ? labelSpacing : 0
        
		Label
		{
			id:			controlLabel
			visible:	controlLabel.text && slider.visible ? true : false
			font:		Theme.font
			color:		enabled ? Theme.textEnabled : Theme.textDisabled
        }
        
		Slider
		{
			id:					control
			Layout.alignment:	control.orientation === Qt.Vertical ? Qt.AlignCenter : Qt.AlignLeft
			Layout.leftMargin:	control.orientation === Qt.Vertical ? leftPadding + Theme.sliderWidth : 0
			value:				0.5
			stepSize:			1 / slider.power
        
			background: Rectangle
			{
				id:				sliderBackground
				x:				control.leftPadding
				y:				control.topPadding
				implicitWidth:	control.vertical ? Theme.sliderWidth : Theme.sliderLength
                implicitHeight: control.vertical ? Theme.sliderLength : Theme.sliderWidth
				width:			control.vertical ? implicitWidth : control.availableWidth
				height:			control.vertical ? control.availableHeight : implicitHeight
				radius:			Theme.sliderWidth / 2
				color:			Theme.sliderPartOn
        
				Rectangle
				{
					width:		control.vertical ? parent.width : control.visualPosition * parent.width
					height:		control.vertical ? control.visualPosition * parent.height : parent.height
					color:		Theme.sliderPartOff
					radius:		Theme.sliderWidth / 2
                }
            }
        
			handle: Rectangle
			{
				id:				sliderHandle
				x:				control.leftPadding + (control.vertical ? sliderBackground.radius - sliderHandle.radius : control.visualPosition * (control.availableWidth - width))
				y:				control.topPadding + (control.vertical ? control.visualPosition * (control.availableHeight - height) : sliderBackground.radius - sliderHandle.radius)
				implicitWidth:	Theme.sliderHandleDiameter
                implicitHeight: Theme.sliderHandleDiameter
				radius:			Theme.sliderHandleDiameter / 2
				color:			control.pressed ? Theme.itemSelectedColor : Theme.controlBackgroundColor
				border.color:	Theme.borderColor
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
        
        JC.DoubleField {
			id:					textField
			value:				control.value
			isBound:			false
			Layout.alignment:	Qt.AlignCenter
			validator:			DoubleValidator { bottom: control.from; top: control.to; decimals: slider.decimals }
            
			onEditingFinished:
			{
                if (control.value != value) {
                    control.value = value;
                    control.moved();
                }
            }
			onTextEdited:
			{
                if (value && !textField.control.acceptableInput)
                    value = control.value
            }
        }
    }    
}
