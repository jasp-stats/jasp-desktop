//
// Copyright (C) 2013-2026 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//
//
//
import QtQuick
import QtQuick.Controls
import QtQuick.Layouts	as L
import JASP.Widgets
import JASP.Controls

PrefsScrollView
{
	id:						scrollPrefs
	

	function resetMe()
	{
		visible = false;
		visible = true;
	}

	MenuHeader
	{
		id:				menuHeader
		headertext:		qsTr("User Interface Options")
		helpMD:			allHelp.PrefsUI
		addMargin:		false

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

		Group
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
				focus:					true
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

				KeyNavigation.tab:	languageGroup
			}
		}
	}

	PrefsLanguage
	{
		id:				languageGroup
		nextTabItem:	altnavcheckbox
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
		title: qsTr("Onboarding")

		CheckBox
		{
			id:					showOnboardingCheckbox
			label:				qsTr("Show onboarding tour on next start")
			checked:			!preferencesModel.onboardingCompleted
			onCheckedChanged:	preferencesModel.onboardingCompleted = !checked
			toolTip:			qsTr("Enable this to see the guided tour again the next time JASP starts.")

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

			KeyNavigation.tab:	startMaximized

		}

		CheckBox
		{
			id:					startMaximized
			label:				qsTr("Start maximized")
			checked:			preferencesModel.startMaximized
			onCheckedChanged:	preferencesModel.startMaximized = checked
			toolTip:			qsTr("Should JASP open its window maximized on startup?")

			KeyNavigation.tab:	disableAnimations

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
