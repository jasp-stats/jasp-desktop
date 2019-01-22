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
			height:				implicitHeight * preferencesModel.uiScale
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
				height:				implicitHeight * preferencesModel.uiScale

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
		height:		uiScaleSlider.height
		width:		uiScaleSlider.x + uiScaleSlider.width

		Text
		{
			id:							uiScaleLabel
			text:						"User Interface Scaling: "
			font:						Theme.font
			color:						Theme.textEnabled
			anchors.verticalCenter:		parent.verticalCenter
		}

		SpinBox
		{
			id:					uiScaleSlider
			value:				preferencesModel.uiScale * _mult
			onValueChanged:		preferencesModel.uiScale = value / _mult
			from:				0.01	* _mult
			to:					3		* _mult
			stepSize:			0.1		* _mult
			font:				Theme.font
			height:				implicitHeight * preferencesModel.uiScale

			property real	_mult:		Math.pow(10, decimals)
			property int	decimals:	2
			property real	realValue:	value / _mult

			validator: DoubleValidator {
			  bottom:	Math.min(uiScaleSlider.from, uiScaleSlider.to)
			  top:		Math.max(uiScaleSlider.from, uiScaleSlider.to)
			}

			textFromValue: function(value, locale)	{  return Number(value / 100).toLocaleString("en-US", 'f', uiScaleSlider.decimals)	}
			valueFromText: function(text, locale)	{  return Number.fromLocaleString("en-US", text) * 100								}

			anchors
			{
				margins:	Theme.generalAnchorMargin
				left:		uiScaleLabel.right
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
