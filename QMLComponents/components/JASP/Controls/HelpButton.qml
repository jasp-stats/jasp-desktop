//
// Copyright (C) 2013-2018 University of Amsterdam
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

import QtQuick
import QtQuick.Layouts
import JASP.Controls

/*!
	\qmltype HelpButton
	\inqmlmodule JASP.Controls 1.0
	\brief A small info button that opens a help page for the current analysis.

	Extends MenuButton with an info icon. When clicked, opens or toggles the specified
	help page in the JASP help panel. Only works within a Form.

	\note HelpButton does not bind to R options. It is a UI-only control.

	\section1 Properties

	\list
	\li \b helpPage (string) - Name of the help page to display. Default: "".
	\endlist

	\section1 Example

	\qml
	HelpButton { helpPage: "anova" }
	\endqml
*/

MenuButton
{
	property string helpPage: ""
	property string helpMD: ""

	iconSource:			enabled ? jaspTheme.iconPath + "info-button.png" : jaspTheme.iconPath + "info-button-black.png" // {info-button, HelpButton-button-grey}.png Icons made by Freepik from https://www.flaticon.com/	radius: 			100 * preferencesModel.uiScale
	buttonPadding:		2 * preferencesModel.uiScale
	implicitHeight:     visible ? 22 * preferencesModel.uiScale : 0
	implicitWidth:      implicitHeight
	Layout.alignment: 	Qt.AlignRight
activeFocusOnTab:	true
	visible:			helpMD || helpPage
	toolTip:			qsTr("Open Documentation")

	onClicked:			if (typeof jaspAnalysis !== "undefined" && helpPage)
							helpModel.showOrToggleParticularPageForAnalysis(jaspAnalysis, helpPage)
						else if (helpPage)
							helpModel.showOrTogglePage(helpPage)
						else if (helpMD)
							helpModel.showOrToggleMarkdown(helpMD)

	onHelpMDChanged: if (helpModel.visible) helpModel.markdown = helpMD

	Accessible.role:			Accessible.Button
	Accessible.name:			text
	Accessible.description:		info === undefined || info == "" ? toolTip !== undefined && toolTip != "" ? toolTip :  qsTr("Helpbutton %1").arg(title) : info
	Accessible.onPressAction:	clicked()
}
