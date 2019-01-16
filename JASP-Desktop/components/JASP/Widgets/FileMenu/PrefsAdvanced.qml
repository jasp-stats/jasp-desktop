import QtQuick			2.11
import QtQuick.Controls 2.4
import QtQuick.Layouts	1.3
import JASP.Widgets		1.0
import JASP.Theme		1.0

Column
{
	anchors.fill:		parent
	anchors.margins:	Theme.generalAnchorMargin
	spacing:			Theme.rowSpacing


	Item
	{
		height:	useDefaultPPICheckbox.height + (editCustomPPI.visible ? editCustomPPI.height : 0 )
		width:	parent.width

		CheckBox
		{
			id:					useDefaultPPICheckbox
			text:				"Use default PPI (Pixels per Inch) in plots: " + preferencesModel.defaultPPI
			checked:			preferencesModel.useDefaultPPI
			onCheckedChanged:	preferencesModel.useDefaultPPI = checked
			font:				Theme.font
		}

		Item
		{
			id:					editCustomPPI
			visible:			!preferencesModel.useDefaultPPI
			width:				customPPISpinBox.x + customPPISpinBox.width
			height:				customPPISpinBox.height
			anchors.top:		useDefaultPPICheckbox.bottom

			Text
			{
				id:						customPPILabel
				text:					"Custom PPI: "
				anchors.left:			parent.left
				anchors.leftMargin:		Theme.subOptionOffset
				anchors.verticalCenter:	parent.verticalCenter
				font:					Theme.font
			}

			SpinBox
			{
				id:					customPPISpinBox
				value:				preferencesModel.customPPI
				onValueChanged:		preferencesModel.customPPI = value
				from:				16
				to:					2000
				stepSize:			16
				editable:			true
				font:				Theme.font

				anchors
				{
					left:			customPPILabel.right
					top:			parent.top
				}
			}
		}
	}

	Item
	{
		height:		uiScaleSlider.y + uiScaleSlider.height
		width:		parent.width

		Text
		{
			id:					uiScaleLabel
			text:				"User Interface Scaling"
			font:				Theme.font
			color:				Theme.textEnabled
		}

		Slider
		{
			id:					uiScaleSlider
			value:				preferencesModel.uiScale
			onValueChanged:		preferencesModel.uiScale = value
			from:				0.1
			to:					3
			stepSize:			0.01

			anchors
			{
				top:		uiScaleLabel.bottom
				margins:	Theme.generalAnchorMargin
				left:		parent.left
				right:		parent.right
			}
		}
	}


	ColumnLayout
	{
		Text
		{
			font:	Theme.font
			text:	qsTr("Image Background Color")
			color:	Theme.textEnabled
		}

		RadioButton
		{
			font:				Theme.font
			text:				qsTr("White")
			checked:			preferencesModel.whiteBackground
			onCheckedChanged:	preferencesModel.whiteBackground = checked
		}

		RadioButton
		{
			font:				Theme.font
			text:				qsTr("Transparent")
			checked:			!preferencesModel.whiteBackground
			onCheckedChanged:	preferencesModel.whiteBackground = !checked
		}
	}
}
