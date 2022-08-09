import QtQuick			2.11
import QtQuick.Controls 2.4
import JASP.Widgets		1.0

import JASP.Controls	1.0

ScrollView
{
	id:						scrollPrefs
	focus:					true
	onActiveFocusChanged:	if(activeFocus) displayExactPVals.forceActiveFocus();
	Keys.onLeftPressed:		resourceMenu.forceActiveFocus();

	Column
	{
		width:			scrollPrefs.width
		spacing:		jaspTheme.rowSpacing

		MenuHeader
		{
			id:				menuHeader
			headertext:		qsTr("Results Preferences")
			helpfile:		"preferences/PrefsResults"
			anchorMe:		false
			width:			scrollPrefs.width - (2 * jaspTheme.generalMenuMargin)
			x:				jaspTheme.generalMenuMargin
		}


		PrefsGroupRect
		{
			title:			qsTr("Table options")

			CheckBox
			{
				id:						displayExactPVals
				label:					qsTr("Display exact p-values")
				checked:				preferencesModel.exactPValues
				onCheckedChanged:		preferencesModel.exactPValues = checked
				KeyNavigation.tab:		useNormalizedNotation
				KeyNavigation.down:		useNormalizedNotation
			}

			CheckBox
			{
				id:						useNormalizedNotation
				label:					qsTr("Use exponent notation") //the default now is normalizedNotation. https://github.com/jasp-stats/INTERNAL-jasp/issues/1872
				checked:				!preferencesModel.normalizedNotation
				onCheckedChanged:		preferencesModel.normalizedNotation = !checked
				KeyNavigation.tab:		fixDecs
				KeyNavigation.down:		fixDecs
			}

			Item
			{
				height:						fixDecs.height
				width:						fixDecs.width + numDecs.width

				CheckBox
				{
					id:						fixDecs
					label:					qsTr("Fix the number of decimals")
					checked:				preferencesModel.fixedDecimals
					onCheckedChanged:		preferencesModel.fixedDecimals = checked
					KeyNavigation.tab:		numDecs
					KeyNavigation.down:		numDecs
				}

				SpinBox
				{
					id:						numDecs
					value:					preferencesModel.numDecimals
					onValueChanged:			preferencesModel.numDecimals = value
					visible:				preferencesModel.fixedDecimals

					KeyNavigation.tab:		useDefaultPPICheckbox
					KeyNavigation.down:		useDefaultPPICheckbox
					anchors
					{
						left:				fixDecs.right
						leftMargin:			jaspTheme.generalAnchorMargin
						verticalCenter:		parent.verticalCenter
					}
				}
			}
		}
		
		PrefsGroupRect
		{
			title:				qsTr("Plot options")

			CheckBox
			{
				id:					useDefaultPPICheckbox
				label:				qsTr("Use PPI of screen in plots: ") + "(" + preferencesModel.defaultPPI + ")"
				checked:			preferencesModel.useDefaultPPI
				onCheckedChanged:	preferencesModel.useDefaultPPI = checked
				height:				implicitHeight * preferencesModel.uiScale
				toolTip:			qsTr("Use the Pixels Per Inch of your screen to render your plots.")
				focus:				true
				KeyNavigation.tab:	customPPISpinBox
				KeyNavigation.down:	customPPISpinBox
			}

			SpinBox
			{
				id:						customPPISpinBox
				value:					preferencesModel.customPPI
				onValueChanged:			preferencesModel.customPPI = value
				from:					16
				to:						2000
				stepSize:				16

				KeyNavigation.tab:		whiteBackgroundButton
				KeyNavigation.down:		whiteBackgroundButton
				text:					qsTr("Custom PPI: ")
				enabled:				!preferencesModel.useDefaultPPI

				x:						jaspTheme.subOptionOffset
			}

			RadioButtonGroup
			{
				title:					qsTr("Image background color")

				RadioButton
				{
					id:					whiteBackgroundButton
					text:				qsTr("White")
					checked:			preferencesModel.whiteBackground
					onCheckedChanged:	preferencesModel.whiteBackground = checked
					toolTip:			qsTr("This makes the background of all plots white, quite useful if you want to use it in LaTeX or submit it to a journal.")
					KeyNavigation.tab:	transparentBackgroundButton
					KeyNavigation.down:	transparentBackgroundButton
				}

				RadioButton
				{
					id:					transparentBackgroundButton
					text:				qsTr("Transparent")
					checked:			!preferencesModel.whiteBackground
					onCheckedChanged:	preferencesModel.whiteBackground = !checked
					toolTip:			qsTr("This makes the background of all plots transparent, quite useful if you want to use it seamlessly on any background that isn't white.")
					KeyNavigation.tab:	displayExactPVals
					KeyNavigation.down:	displayExactPVals
				}
			}
		}

		Item
		{
			id:		extraSpaceForScrolling
			width:	1
			height:	1
		}
	}
}
