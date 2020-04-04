import QtQuick			2.12
import QtQuick.Controls 2.12
import JASP.Widgets		1.0
import JASP.Controls	1.0

ScrollView
{
	id:						scrollPrefs
	focus:					true
	onActiveFocusChanged:	if(activeFocus) uiScaleSpinBox.forceActiveFocus();
	Keys.onLeftPressed:		resourceMenu.forceActiveFocus();

	function resetMe()
	{
		visible = false;
		visible = true;
	}

	Connections
	{
		target:						preferencesModel
		onCurrentThemeNameChanged:	scrollPrefs.resetMe();
	}

	Connections
	{
		target:						languageModel
		onCurrentIndexChanged:		scrollPrefs.resetMe();
	}

	Column
	{
		width:			scrollPrefs.width
		spacing:		jaspTheme.rowSpacing

		MenuHeader
		{
			id:				menuHeader
			headertext:		qsTr("User Interface options")
			helpfile:		"preferences/prefsui"
			anchorMe:		false
			width:			scrollPrefs.width - (2 * jaspTheme.generalMenuMargin)
			x:				jaspTheme.generalMenuMargin
		}

		PrefsGroupRect
		{
			title:		qsTr("Themes")

			RadioButtonGroup
			{
				id:			themes

				RadioButton
				{
					id:					lightThemeButton
					label:				qsTr("Light Theme")
					checked:			preferencesModel.currentThemeName === "lightTheme"
					onCheckedChanged:	preferencesModel.currentThemeName  =  "lightTheme"
					toolTip:			qsTr("Switches to a light theme, this is the default and original flavour of JASP.")
					KeyNavigation.tab:	darkThemeButton
					KeyNavigation.down:	darkThemeButton
				}

				RadioButton
				{
					id:					darkThemeButton
					label:				qsTr("Dark Theme")
					checked:			preferencesModel.currentThemeName === "darkTheme"
					onCheckedChanged:	preferencesModel.currentThemeName  =  "darkTheme"
					toolTip:			qsTr("Switches to a dark theme, makes JASP a lot easier on the eyes for those night owls out there.")
					KeyNavigation.tab:	languages
					KeyNavigation.down:	languages
				}
			}
		}

		PrefsGroupRect
		{
			id:		languageGroup
			title:	qsTr("Preferred Language")

			ComboBox
			{
				id:						languages
				fieldWidth:				100

				label:					qsTr("Choose Language  ")
				valueRole:				"label"

				useModelDefinedIcon:	false

				currentIndex:			languageModel.currentIndex
				onActivated:			languageModel.changeLanguage(index);

				model:					languageModel

				KeyNavigation.tab:		uiScaleSpinBox
				KeyNavigation.down:		uiScaleSpinBox

			}

		}

		PrefsGroupRect
		{
			title: qsTr("Miscellaneous options")

			SpinBox
			{
				id:						uiScaleSpinBox
				value:					Math.round(preferencesModel.uiScale * 100)
				onEditingFinished:		preferencesModel.uiScale = value / 100
				from:					20
				to:						300
				stepSize:				10
				decimals:				0
				text:					qsTr("Zoom (%): ")
				toolTip:				qsTr("Increase or decrease the size of the interface elements (text, buttons, etc).")

				KeyNavigation.tab:		uiMaxFlickVelocity
				KeyNavigation.down:		uiMaxFlickVelocity

				widthLabel:				Math.max(uiScaleSpinBox.implicitWidthLabel, uiMaxFlickVelocity.implicitWidthLabel)
			}

			SpinBox
			{
				id:						uiMaxFlickVelocity
				value:					preferencesModel.maxFlickVelocity
				onValueChanged:			if(value !== "") preferencesModel.maxFlickVelocity = value
				from:					100
				to:						3000
				stepSize:				100
				decimals:				0
				text:					qsTr("Scroll speed (pix/s): ")
				toolTip:				qsTr("Set the speed with which you can scroll in the options, dataviewer and other places.")
				widthLabel:				uiScaleSpinBox.widthLabel

				KeyNavigation.tab:		safeGraphicsMode
				KeyNavigation.down:		safeGraphicsMode
			}


			CheckBox
			{
				id:					safeGraphicsMode
				label:				qsTr("Safe Graphics Mode")
				checked:			preferencesModel.safeGraphics
				onCheckedChanged:	preferencesModel.safeGraphics = checked
				toolTip:			qsTr("Switches to a \"safer\" mode for graphics aka software rendering.\nIt will make your interface slower but if you have some problems (weird glitches, cannot see results or anything even) might fix them.\nAnalyses will still be just as fast though.")


				KeyNavigation.tab:		disableAnimations
				KeyNavigation.down:		disableAnimations

			}

			CheckBox
			{
				id:					disableAnimations
				label:				qsTr("Disable Animations")
				checked:			preferencesModel.disableAnimations
				onCheckedChanged:	preferencesModel.disableAnimations = checked
				toolTip:			enabled ? qsTr("Turns off all animations, this is implied when \"Safe Graphics Mode\" is on.") : qsTr("Already disabled animations because \"Safe Graphics Mode\" is on")

				enabled:			!preferencesModel.safeGraphics

				KeyNavigation.tab:		useNativeFileDialog
				KeyNavigation.down:		useNativeFileDialog
			}


			CheckBox
			{
				id:					useNativeFileDialog
				label:				qsTr("Use Native File Dialogs")
				checked:			preferencesModel.useNativeFileDialog
				onCheckedChanged:	preferencesModel.useNativeFileDialog = checked
				toolTip:			qsTr("If disabled it will not use your operating system's file dialogs but those made by Qt. This might solve some problems on Windows where JASP crashes on pressing \"Browse\".")

				KeyNavigation.tab:		lightThemeButton
				KeyNavigation.down:		lightThemeButton
			}
		}
	}
}
