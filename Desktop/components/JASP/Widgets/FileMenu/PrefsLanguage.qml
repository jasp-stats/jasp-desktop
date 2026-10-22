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
import QtQuick
import QtQuick.Controls
import QtQuick.Layouts	as L
import JASP.Widgets
import JASP.Controls

PrefsGroupRect
{
	id:		languageGroup
	title:	qsTr("Preferred language")

	property bool showHelpLink: true
	property var nextTabItem
	


	DropDown
	{
		id:							languages
		label:						qsTr("Choose language")
		source:						languageModel
		startValue: 				languageModel.currentLanguage
		onValueChanged: 			languageModel.currentLanguage = value

		KeyNavigation.tab: 			useThousandsSeparator

	}

	CheckBox
	{
		id:							useThousandsSeparator
		label:						qsTr("Use thousands separators")
		checked:					languageModel.useThousandSeps
		onCheckedChanged:			languageModel.useThousandSeps = checked
		toolTip:					qsTr("Disable to remove thousands separators from all numbers.")

		KeyNavigation.tab:			useAlternativeLocale
	}

	CheckBox
	{
		id:							useAlternativeLocale
		label:						qsTr("Use an alternative language and territory dependent display for numbers")
		checked:					languageModel.useAlternativeLocale
		onCheckedChanged:			languageModel.useAlternativeLocale = checked
		toolTip:					qsTr("Use the locale specified below for display of numbers and currency.") //dates and times will have to be added later

		KeyNavigation.tab:			alternativeLocaleTerritory
	}

	RowLayout
	{
		spacing:		jaspTheme.generalAnchorMargin

		Group
		{
			enabled:					languageModel.useAlternativeLocale

			DropDown
			{
				id:							alternativeLocaleLanguage
				label:						qsTr("Alt. language")
				values:		 				languageModel.altLanguages
				startValue:				 	languageModel.currentAltLanguage
				onValueChanged:				if(value != "") languageModel.currentAltLanguage = value
				addEmptyValue:				false
				KeyNavigation.tab:			alternativeLocaleTerritory
				control.width:				Math.max(alternativeLocaleLanguage.control.implicitWidth, alternativeLocaleTerritory.control.implicitWidth)
			}

			DropDown
			{
				id:							alternativeLocaleTerritory
				label:						qsTr("Alt. territory")
				values:		 				languageModel.altTerritories
				startValue:				 	languageModel.currentAltTerritory
				value:						languageModel.currentAltTerritory
				onValueChanged:				if(value != "") languageModel.currentAltTerritory = value
				addEmptyValue:				false
				KeyNavigation.tab:			nextTabItem ? nextTabItem : undefined
				control.width:				Math.max(alternativeLocaleLanguage.control.implicitWidth, alternativeLocaleTerritory.control.implicitWidth)
			}

		}

		Item{ width: jaspTheme.generalAnchorMargin}

		Rectangle
		{
			implicitWidth:		exampleFormattingText.implicitWidth
			implicitHeight:		exampleFormattingText.implicitHeight

			color:				jaspTheme.white
			border.width:		1
			border.color:		jaspTheme.borderColor
			radius:				jaspTheme.borderRadius

			Text
			{
				id:					exampleFormattingText

				text:				languageModel.exampleFormatting
				color:				jaspTheme.textEnabled
				font.pixelSize:		Math.round(12 * preferencesModel.uiScale)
				font.family:		preferencesModel.interfaceFont
				wrapMode:			Text.WordWrap
				padding:			jaspTheme.itemPadding
			}
		}
	}


	Text
	{
		id:					translationDocLink

		text:				qsTr("Help us translate or improve JASP in your language")
		color:				jaspTheme.blue
		font.pixelSize:		Math.round(12 * preferencesModel.uiScale)
		font.family:			preferencesModel.interfaceFont
		font.underline:		true
		visible:			showHelpLink && mainWindow.externalLinksEnabled

		MouseArea
		{
			id:				mouseAreaTranslationDocLink
			anchors.fill:	parent
			onClicked:		Qt.openUrlExternally("https://jasp-stats.org/translation-guidelines")
			cursorShape:	Qt.PointingHandCursor
		}
	}

}
