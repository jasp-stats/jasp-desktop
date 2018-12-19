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

import QtQuick 2.11
import QtQuick.Controls 2.4
import JASP.Theme 1.0

Rectangle {
	id        : jaspRibbon
	objectName: "jaspRibbon"
	width     : 500
	height    : 80
	color	  : Theme.uiBackground

	signal ribbonButtonClicked(variant clusterButton)

	function dispatchButtonClickSignal(clusterButton) {
		jaspRibbon.ribbonButtonClicked(clusterButton)
	}

	Row {
		id        : ribbonRow
		objectName: "ribbonRow"
		padding   : 10
		spacing   : 10

		Repeater {
			id      : tabs
			model   : ribbonModel.currentButtonModel
			delegate: Loader {
				sourceComponent	: displayText === "???" ? toolSeparator : ribbonButtonDelegate

				Component {
					id: ribbonButtonDelegate

					RibbonButton {
						text   : displayText
						source : (ribbonModel.currentButtonModel.isDynamic() ? "file:" : "qrc:/icons/") + iconSource
						menu   : analysisMenu
						enabled: ribbonEnabled
					}
				}

				Component {	id: toolSeparator;	ToolSeparator { height: Theme.ribbonButtonHeight } }
			}
		}
	}
}
