import QtQuick 2.11
import QtQuick.Controls 2.4
import QtQuick.Layouts 1.3
import JASP.Theme 1.0

JASPControl {
	id:					comboBox
	controlType:		"ComboBox"
	implicitHeight:		control.height
	implicitWidth:		control.implicitWidth + (label.visible ? labelSpacing + label.implicitWidth : 0)
	width:				implicitWidth
	controlBackground:	comboBoxBackground

    
	property int	labelSpacing:			4 * preferencesModel.uiScale
	property alias	label:					label
    property alias	text:					label.text
	property string	currentText				//Am i empty or what?
	property string	currentColumnType		//Same here
	property alias	currentIndex:			control.currentIndex
	property alias	indexDefaultValue:		control.currentIndex
	property alias	model:					control.model
	property alias	values:					control.model
	property string	textRole:				"title"
	property string	valueRole:				"value"
	property bool	showVariableTypeIcon:	false
	property var	syncModels				//defaults would be nice
	property bool	addEmptyValue:			false
	property string	emptyValue:				qsTr("<no choice>")
	property alias	control:				control
	property bool	initialized:			false
    
    signal activated(int index);
    
	function resetWidth(value)
	{
        textMetrics.font = control.font
        textMetrics.text = value
		var newWidth = textMetrics.width + ((comboBox.showVariableTypeIcon ? 20 : 4) * preferencesModel.uiScale);
		if (newWidth > control.modelWidth)
		{
            control.modelWidth = newWidth;
            comboBox.width = comboBox.implicitWidth; // the width is not automatically updated by the implicitWidth...
        }
    }
    
	Component.onCompleted: control.activated.connect(activated);
    
	RowLayout
	{
        spacing: label.visible ? labelSpacing : 0
        
		Label
		{
            id: label
            visible: label.text && comboBox.visible ? true : false
        }
        
        ComboBox {
							id:				control
							focus:			true
							spacing:		5 * preferencesModel.uiScale
							height:			Theme.comboBoxHeight
			property int	modelWidth:		30 * preferencesModel.uiScale
			property bool	isEmptyValue:	comboBox.addEmptyValue && currentIndex <= 0
							implicitWidth:	modelWidth + leftPadding + rightPadding + canvas.width
							textRole:		comboBox.textRole
            
			TextMetrics { id: textMetrics }

			delegate: ItemDelegate
			{
				height:								control.height
				highlighted:						control.highlightedIndex === index

				contentItem:	Rectangle
				{
					id:								itemRectangle
					anchors.fill:					parent

					property bool isEmptyValue:		comboBox.addEmptyValue && index == 0

					Image
					{
						id:							delegateIcon
						x:							1 * preferencesModel.uiScale
						height:						15 * preferencesModel.uiScale
						width:						15 * preferencesModel.uiScale
						source:						(visible && comboBox.initialized) ? iconFiles[model.columnType] : ""
						visible:					comboBox.showVariableTypeIcon && !itemRectangle.isEmptyValue

						anchors.verticalCenter:		parent.verticalCenter
                    }
                    
                    Text {
						x:							(delegateIcon.visible ? 20 : 4) * preferencesModel.uiScale
						text:						comboBox.initialized ? model.name : ""
						font:						control.font
						color:						itemRectangle.isEmptyValue ? Theme.grayDarker : Theme.black
						verticalAlignment:			Text.AlignVCenter
						anchors.horizontalCenter:	itemRectangle.isEmptyValue ? parent.horizontalCenter : undefined
                    }

                }


            }
            
            
			indicator: Canvas
			{
				id:				canvas
				x:				control.width - width - control.spacing
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
        

			contentItem: Item
			{
				Image
				{
					id:						contentIcon
					height:					15 * preferencesModel.uiScale
					width:					15 * preferencesModel.uiScale
					x:						3  * preferencesModel.uiScale
                    anchors.verticalCenter: parent.verticalCenter
					source:					visible ? iconFiles[comboBox.currentColumnType] : ""
					visible:				comboBox.showVariableTypeIcon && comboBox.currentColumnType && !control.isEmptyValue
                }
                
				Text
				{
					x:							(contentIcon.visible ? 23 : 4) * preferencesModel.uiScale
					text:						comboBox.currentText
					font:						control.font
					anchors.verticalCenter:		parent.verticalCenter
					anchors.horizontalCenter:	control.isEmptyValue ? parent.horizontalCenter : undefined
					color:						(!enabled || control.isEmptyValue) ? Theme.grayDarker : Theme.black
                }
            }

			background: Rectangle
			{
				id:				comboBoxBackground
                implicitHeight: control.height
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
        }
    }
}
