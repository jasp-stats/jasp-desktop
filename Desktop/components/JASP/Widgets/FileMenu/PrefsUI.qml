import QtQuick			2.12
import QtQuick.Controls 2.12
import QtQuick.Layouts	1.3 as L
import JASP.Widgets		1.0
import JASP.Controls	1.0

ScrollView
{
	id:						scrollPrefs
	focus:					true
	onActiveFocusChanged:	if(activeFocus) interfaceFonts.forceActiveFocus();
	Keys.onLeftPressed:		resourceMenu.forceActiveFocus();
	hoverEnabled:			false

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
		z:				100

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

			Item
			{
				visible: false;
				// If the defaultInterfaceFont etc does not exist on the machine, then the default font of the machine is used.
				// These (invisible) Text items are just to ask what will be the real font used.
				
				Text
				{
					
					id:				 defaultInterfaceFont
					font.family:	 preferencesModel.defaultInterfaceFont
					text:			 fontInfo.family
				}
				
				Text
				{
					id:			 	defaultRCodeFont
					text:			fontInfo.family
					font.family:	preferencesModel.defaultCodeFont
				}
				
				Text
				{
					id: 			defaultResultFont
					text: 			fontInfo.family
					font.family: 	preferencesModel.defaultResultFont
				}
			}

			GroupBox
			{
				width:			parent.width
				
				DropDown 
				{
					id:			 			interfaceFonts
					label:					qsTr("Interface:")
					values:			 		preferencesModel.allInterfaceFonts
					addEmptyValue:			true
					showEmptyValueAsNormal:	true
					addLineAfterEmptyValue:	true
					placeholderText:		qsTr("default: %1").arg(defaultInterfaceFont.fontInfo.family)
					startValue:				preferencesModel.interfaceFont
					onValueChanged: 		preferencesModel.interfaceFont = (currentIndex <= 0 ? "" : value)

					KeyNavigation.tab:		codeFonts
				}
			
				DropDown
				{
					id:							codeFonts
					label:						qsTr("R, JAGS, or lavaan code:")
					values:		 				preferencesModel.allCodeFonts
					addEmptyValue:		 		true
					showEmptyValueAsNormal:		true
					addLineAfterEmptyValue:		true
					placeholderText:		 	qsTr("default: %1").arg(defaultRCodeFont.fontInfo.family)
					startValue:				 	preferencesModel.codeFont
					onValueChanged:				preferencesModel.codeFont = (currentIndex <= 0 ? "" : value)

					KeyNavigation.tab:			resultFonts
				}

				DropDown
				{
					id:							resultFonts
					label:						qsTr("Result & help:")
					values:						preferencesModel.allResultFonts
					addEmptyValue:				true
					showEmptyValueAsNormal:		true
					addLineAfterEmptyValue:		true
					placeholderText: 			qsTr("default: %1").arg(defaultResultFont.fontInfo.family)
					startValue: 				preferencesModel.resultFont
					onValueChanged: 			preferencesModel.resultFont = (currentIndex <= 0 ? "" : value)

					KeyNavigation.tab: 			qtTextRendering
				}
			}
			

			CheckBox
			{
				id:					qtTextRendering
				label:				qsTr("Use Qt's text rendering")
				checked:			preferencesModel.guiQtTextRender
				onCheckedChanged:	preferencesModel.guiQtTextRender = checked
				toolTip:			qsTr("If disabled will switch the textrendering to native.")


				KeyNavigation.tab:		lightThemeButton
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
					onCheckedChanged:	if (checked) preferencesModel.currentThemeName  =  "lightTheme"
					toolTip:			qsTr("Switches to a light theme, this is the default and original flavour of JASP.")

					KeyNavigation.tab:		darkThemeButton
				}

				RadioButton
				{
					id:					darkThemeButton
					label:				qsTr("Dark theme")
					checked:			preferencesModel.currentThemeName === "darkTheme"
					onCheckedChanged:	if (checked) preferencesModel.currentThemeName  =  "darkTheme"
					toolTip:			qsTr("Switches to a dark theme, makes JASP a lot easier on the eyes for those night owls out there.")
					
					KeyNavigation.tab:		languages
				}
			}
		}

		PrefsGroupRect
		{
			id:		languageGroup
			title:	qsTr("Preferred language")
			
			
			DropDown
			{
				id:							languages
				label:						qsTr("Choose language  ")
				source:						languageModel
				startValue: 				languageModel.currentLanguage
				onValueChanged: 			languageModel.currentLanguage = value

				KeyNavigation.tab: 			altnavcheckbox
				
			}
			

			Text
			{
				id:					translationDocLink

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
			title: qsTr("Accessibility options")


			CheckBox
			{
				id:					altnavcheckbox
				label:				qsTr("ALT-Navigation mode")
				checked:			preferencesModel.ALTNavModeActive
				onCheckedChanged:	preferencesModel.ALTNavModeActive = checked
				toolTip:			qsTr("Whether ALT-Navigation mode is active or not.")

				KeyNavigation.tab:	checkForUpdates
			}
		}
		
		PrefsGroupRect
		{
			title: qsTr("Check for updates")
			
			CheckBox
			{
				id:					checkForUpdates
				label:				qsTr("Daily automatic check for updates & known issues")
				checked:			preferencesModel.checkUpdates
				onCheckedChanged:	preferencesModel.checkUpdates = checked
				toolTip:			qsTr("JASP doesn't share any of your data when it gets updates, not even which version of JASP you are using.\nIt does share your IP-address with the server but that is required for internet to function.\n\nThe list of known issues it downloads is rarely used, mostly the issues are at [jasp-issues](https://github.com/jasp-stats/jasp-issues/issues). However if we realize a terrible error has slipped into an analysis this will show you *in the analysis* that there is something you should take into account. Luckily we almost never need to use it.")

				KeyNavigation.tab:	uiScaleSpinBox

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
				KeyNavigation.tab:		ribbonBarSpinBox

				widthLabel:				Math.max(uiScaleSpinBox.implicitWidthLabel,Math.max(ribbonBarSpinBox.implicitWidthLabel, uiMaxFlickVelocity.implicitWidthLabel))
			}
			
			SpinBox
			{
				id:						ribbonBarSpinBox
				value:					Math.round(preferencesModel.ribbonBarHeightScale * 100)
				onValueChanged:			if(value!= "") preferencesModel.ribbonBarHeightScale = value / 100
				from:					10
				to:						500
				stepSize:				10
				decimals:				0
				text:					qsTr("Ribbon scale (%): ")
				toolTip:				qsTr("Set the scale of height of the ribbon.")
				KeyNavigation.tab:		uiMaxFlickVelocity

				widthLabel:				uiScaleSpinBox.widthLabel
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
			}


			CheckBox
			{
				id:					safeGraphicsMode
				label:				qsTr("Safe graphics mode")
				checked:			preferencesModel.safeGraphics
				onCheckedChanged:	preferencesModel.safeGraphics = checked
				toolTip:			qsTr("Switches to a \"safer\" mode for graphics aka software rendering.\nIt will make your interface slower but if you have some problems (weird glitches, cannot see results or anything even) might fix them.\nAnalyses will still be just as fast though.")
				
				KeyNavigation.tab:			disableAnimations

			}

			CheckBox
			{
				id:					disableAnimations
				label:				qsTr("Disable animations")
				checked:			preferencesModel.disableAnimations
				onCheckedChanged:	preferencesModel.disableAnimations = checked
				toolTip:			enabled ? qsTr("Turns off all animations, this is implied when \"Safe Graphics Mode\" is on.") : qsTr("Already disabled animations because \"Safe Graphics Mode\" is on")

				enabled:			!preferencesModel.safeGraphics

				KeyNavigation.tab:			useNativeFileDialog
			}


			CheckBox
			{
				id:					useNativeFileDialog
				label:				qsTr("Use native file dialogs")
				checked:			preferencesModel.useNativeFileDialog
				onCheckedChanged:	preferencesModel.useNativeFileDialog = checked
				toolTip:			qsTr("If disabled it will not use your operating system's file dialogs but those made by Qt. This might solve some problems on Windows where JASP crashes on pressing \"Browse\".")

				KeyNavigation.tab:	reportingMode

			}

			CheckBox
			{
				id:					reportingMode
				label:				qsTr("Reporting mode")
				checked:			preferencesModel.reportingMode
				onCheckedChanged:	preferencesModel.reportingMode = checked
				toolTip:			qsTr("Whether JASP should run in reporting mode or not.")
				visible:			preferencesModel.developerMode

				KeyNavigation.tab:	interfaceFonts
			}
		}
	}
}
