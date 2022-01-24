import QtQuick			2.12
import QtQuick.Controls 2.12
import QtQuick.Layouts	1.3 as L
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
		target:				preferencesModel
		function onCurrentThemeNameChanged(name)	{ scrollPrefs.resetMe(); }
	}

	Connections
	{
		target:				languageModel
		function onCurrentLanguageChanged()			{ scrollPrefs.resetMe(); }
	}

	Column
	{
		width:			scrollPrefs.width
		spacing:		jaspTheme.rowSpacing

		MenuHeader
		{
			id:				menuHeader
			headertext:		qsTr("User Interface Options")
			helpfile:		"preferences/PrefsUI"
			anchorMe:		false
			width:			scrollPrefs.width - (2 * jaspTheme.generalMenuMargin)
			x:				jaspTheme.generalMenuMargin
		}

		PrefsGroupRect
		{
			id:		fontGroup
			title:	qsTr("Fonts")

			GridLayout
			{
				columns			: 2
				rowSpacing		: 3 * preferencesModel.uiScale
				columnSpacing	: 3 * preferencesModel.uiScale

				Text { text: qsTr("Interface:") }

				DropDown
				{
					id						: interfaceFonts
					values					: preferencesModel.allInterfaceFonts
					addEmptyValue			: true
					showEmptyValueAsNormal	: true
					addLineAfterEmptyValue	: true
					addScrollBar			: true
					placeholderText			: qsTr("default: %1").arg(defaultInterfaceFont.fontInfo.family)
					startValue				: preferencesModel.interfaceFont
					onValueChanged			: preferencesModel.interfaceFont = (currentIndex <= 0 ? "" : value)

					KeyNavigation.tab		: codeFonts
					KeyNavigation.down		: codeFonts

					Text
					{
						// If the defaultInterfaceFont does not exist on the machine, then the default font of the machine is used.
						// This (invisible) Text item is just to ask what will be the real font used.
						id					: defaultInterfaceFont
						font.family			: preferencesModel.defaultInterfaceFont
						text				: fontInfo.family
						visible				: false
					}
				}

				Text { text: qsTr("R, JAGS, or lavaan code:") }

				DropDown
				{
					id						: codeFonts
					values					: preferencesModel.allCodeFonts
					addEmptyValue			: true
					showEmptyValueAsNormal	: true
					addLineAfterEmptyValue	: true
					addScrollBar			: true
					placeholderText			: qsTr("default: %1").arg(defaultRCodeFont.fontInfo.family)
					startValue				: preferencesModel.codeFont
					onValueChanged			: preferencesModel.codeFont = (currentIndex <= 0 ? "" : value)

					KeyNavigation.tab		: resultFonts
					KeyNavigation.down		: resultFonts

					Text
					{
						id					: defaultRCodeFont
						text				: fontInfo.family
						font.family			: preferencesModel.defaultCodeFont
						visible				: false
					}

				}

				Text { text: qsTr("Result & help:") }

				DropDown
				{
					id						: resultFonts
					values					: preferencesModel.allResultFonts
					addEmptyValue			: true
					showEmptyValueAsNormal	: true
					addLineAfterEmptyValue	: true
					addScrollBar			: true
					placeholderText			: qsTr("default: %1").arg(defaultResultFont.fontInfo.family)
					startValue				: preferencesModel.resultFont
					onValueChanged			: preferencesModel.resultFont = (currentIndex <= 0 ? "" : value)

					KeyNavigation.tab		: lightThemeButton
					KeyNavigation.down		: lightThemeButton

					Text
					{
						id					: defaultResultFont
						text				: fontInfo.family
						font.family			: preferencesModel.defaultResultFont
						visible				: false
					}
				}
			}
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
					label:				qsTr("Light theme")
					checked:			preferencesModel.currentThemeName === "lightTheme"
					onCheckedChanged:	preferencesModel.currentThemeName  =  "lightTheme"
					toolTip:			qsTr("Switches to a light theme, this is the default and original flavour of JASP.")
					KeyNavigation.tab:	darkThemeButton
					KeyNavigation.down:	darkThemeButton
				}

				RadioButton
				{
					id:					darkThemeButton
					label:				qsTr("Dark theme")
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
			title:	qsTr("Preferred language")

			ComboBox
			{
				id:						languages
				fieldWidth:				100

				label:					qsTr("Choose language  ")

				currentValue:			languageModel.currentLanguage
				onCurrentValueChanged:	languageModel.currentLanguage = currentValue;

				source:					languageModel

				KeyNavigation.tab:		uiScaleSpinBox
				KeyNavigation.down:		uiScaleSpinBox
			}

			Text
			{
                id:                     translationDocLink

				text:				qsTr("Help us translate or improve JASP in your language")
				color:				jaspTheme.blue
				font.pixelSize:		Math.round(12 * preferencesModel.uiScale)
				font.family:		preferencesModel.interfaceFont
				font.underline:		true

				MouseArea
				{
					id:				mouseAreaTranslationDocLink
					anchors.fill:	parent
                    onClicked:		Qt.openUrlExternally("https://jasp-stats.org/translation-guidelines")
					cursorShape:	Qt.PointingHandCursor
				}
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
				label:				qsTr("Safe graphics mode")
				checked:			preferencesModel.safeGraphics
				onCheckedChanged:	preferencesModel.safeGraphics = checked
				toolTip:			qsTr("Switches to a \"safer\" mode for graphics aka software rendering.\nIt will make your interface slower but if you have some problems (weird glitches, cannot see results or anything even) might fix them.\nAnalyses will still be just as fast though.")


				KeyNavigation.tab:		disableAnimations
				KeyNavigation.down:		disableAnimations

			}

			CheckBox
			{
				id:					disableAnimations
				label:				qsTr("Disable animations")
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
				label:				qsTr("Use native file dialogs")
				checked:			preferencesModel.useNativeFileDialog
				onCheckedChanged:	preferencesModel.useNativeFileDialog = checked
				toolTip:			qsTr("If disabled it will not use your operating system's file dialogs but those made by Qt. This might solve some problems on Windows where JASP crashes on pressing \"Browse\".")

				KeyNavigation.tab:		interfaceFonts
				KeyNavigation.down:		interfaceFonts
			}
		}
	}
}
