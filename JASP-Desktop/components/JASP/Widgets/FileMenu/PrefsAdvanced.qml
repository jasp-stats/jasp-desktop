import QtQuick			2.11
import QtQuick.Controls 2.4
import QtQuick.Layouts	1.3
import JASP.Widgets		1.0
import JASP.Theme		1.0
import JASP.Controls	1.0
import JASP				1.0

Item {

	anchors.fill:		parent

	MenuHeader {
		id: menuHeader
		headertext:"Advanced Preferences"
		helpbutton: true
		helpfile: "preferences/prefsadvanced"
	}

	ScrollView
	{
		id:				scrollPrefs
		anchors.top:	menuHeader.bottom
		anchors.left:	menuHeader.left
		anchors.right:	menuHeader.right
		anchors.bottom: parent.bottom
		anchors.topMargin: 2 * Theme.generalMenuMargin

		Column
		{
			width:			scrollPrefs.width
			spacing:		Theme.rowSpacing

			Item
			{
				height:	useDefaultPPICheckbox.height + (editCustomPPI.visible ? editCustomPPI.height : 0 )
				width:	parent.width

				CheckBox
				{
					id:					useDefaultPPICheckbox
					label:				"Use PPI of screen in plots: " + preferencesModel.defaultPPI
					checked:			preferencesModel.useDefaultPPI
					onCheckedChanged:	preferencesModel.useDefaultPPI = checked
					//font:				Theme.font
					height:				implicitHeight * preferencesModel.uiScale
					toolTip:			qsTr("Use the Pixels per Inch of your screen to render your plots")
				}

				Item
				{
					id:					editCustomPPI
					visible:			!preferencesModel.useDefaultPPI
					width:				customPPISpinBox.x + customPPISpinBox.width
					height:				customPPISpinBox.height
					anchors.top:		useDefaultPPICheckbox.bottom
					anchors.topMargin:	Theme.generalAnchorMargin

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
						height:				Theme.spinBoxHeight	//implicitHeight * preferencesModel.uiScale
						x: uiScaleSoinBox.x

						anchors
						{
							//left:			customPPILabel.right
							verticalCenter:	parent.verticalCenter
						}
					}
				}
			}

			Item
			{
				height:		uiScaleSoinBox.height
				width:		uiScaleSoinBox.x + uiScaleSoinBox.width
				anchors.topMargin:	Theme.generalAnchorMargin

				Text
				{
					id:							uiScaleLabel
					text:						"User Interface Scaling: "
					font:						Theme.font
					color:						Theme.textEnabled
					anchors.verticalCenter:		parent.verticalCenter
					//toolTip:					qsTr("Change the scale of the entire interface, can also be done through Ctrl/Cmd + '+' or '-'")
				}

				SpinBox
				{
					id:					uiScaleSoinBox
					value:				preferencesModel.uiScale * _mult
					onValueChanged:		preferencesModel.uiScale = value / _mult
					from:				0.01	* _mult
					to:					3		* _mult
					stepSize:			0.1		* _mult
					font:				Theme.font
					height:				Theme.spinBoxHeight


					property real	_mult:		Math.pow(10, decimals)
					property int	decimals:	2
					property real	realValue:	value / _mult

					validator: JASPDoubleValidator {
						bottom:	Math.min(uiScaleSoinBox.from, uiScaleSoinBox.to)
						top:		Math.max(uiScaleSoinBox.from, uiScaleSoinBox.to)
						decimals: uiScaleSoinBox.decimals
					}

					textFromValue: function(value, locale)	{  return Number(value / 100).toLocaleString("en-US", 'f', uiScaleSoinBox.decimals)	}
					valueFromText: function(text, locale)	{  return Number.fromLocaleString("en-US", text) * 100								}

					anchors
					{
						margins:	Theme.generalAnchorMargin
						left:		uiScaleLabel.right
					}
				}
			}


			RadioButtonGroup
			{
				title: qsTr("Image Background Color")

				RadioButton
				{
					//font:				Theme.font
					text:				qsTr("White")
					checked:			preferencesModel.whiteBackground
					onCheckedChanged:	preferencesModel.whiteBackground = checked
					toolTip:			"This makes the background of all plots white, quite useful if you want to use it in LaTeX or submit it to a journal."
				}

				RadioButton
				{
					//font:				Theme.font
					text:				qsTr("Transparent")
					checked:			!preferencesModel.whiteBackground
					onCheckedChanged:	preferencesModel.whiteBackground = !checked
					toolTip:			"This makes the background of all plots transparent, quite useful if you want to use it seamlessly on any background that isn't white."
				}
			}

			Item
			{
				height:	developerMode.height + (editDeveloperFolder.visible ? editDeveloperFolder.height : 0)
				width:	parent.width - Theme.generalAnchorMargin

				CheckBox
				{
					id:					developerMode
					label:				qsTr("Developer mode")
					checked:			preferencesModel.developerMode
					onCheckedChanged:	preferencesModel.developerMode = checked
					toolTip:			"To use JASP Modules enable this option"
				}

				Item
				{
					id:					editDeveloperFolder
					visible:			preferencesModel.developerMode
					width:				parent.width
					height:				browseDeveloperFolderButton.height + overwriteDescriptionEtc.height
					anchors.top:		developerMode.bottom


					RectangularButton
					{
						id:					browseDeveloperFolderButton
						text:				qsTr("Select developer folder")
						onClicked:			preferencesModel.browseDeveloperFolder()
						anchors.left:		parent.left
						anchors.leftMargin: Theme.subOptionOffset
						toolTip:			qsTr("Browse to your JASP Module folder")
					}

					Rectangle
					{
						id:					developerFolderTextRect
						anchors
						{
							left:			browseDeveloperFolderButton.right
							right:			parent.right
							top:			parent.top
						}

						height:				browseDeveloperFolderButton.height
						color:				Theme.white
						border.color:		Theme.buttonBorderColor
						border.width:		1

						TextInput
						{
							id:					developerFolderText
							text:				preferencesModel.developerFolder
							clip:				true
							font:				Theme.font
							onTextChanged:		preferencesModel.developerFolder = text
							color:				Theme.textEnabled

							anchors
							{
								left:			parent.left
								right:			parent.right
								verticalCenter:	parent.verticalCenter
								margins:		Theme.generalAnchorMargin
							}

							Connections
							{
								target:					preferencesModel
								onCustomEditorChanged:	developerFolderText = preferencesModel.developerFolder
							}

						}
					}

					CheckBox
					{
						id:					overwriteDescriptionEtc
						label:				qsTr("Regenerate package metadata every time (DESCRIPTION & NAMESPACE)")
						checked:			preferencesModel.devModRegenDESC
						onCheckedChanged:	preferencesModel.devModRegenDESC = checked
						//font:				Theme.font
						height:				implicitHeight * preferencesModel.uiScale
						toolTip:			qsTr("Disable this option if you are transforming your R-package to a JASP Module or simply want to keep manual changes to DESCRIPTION and NAMESPACE.")

						anchors
						{
							left:			parent.left
							leftMargin:		Theme.subOptionOffset
							top:			developerFolderTextRect.bottom
						}
					}
				}
			}
		}
	}
}
