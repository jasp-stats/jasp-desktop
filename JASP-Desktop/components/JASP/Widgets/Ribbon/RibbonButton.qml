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
import QtGraphicalEffects 1.0
import JASP.Theme 1.0

Rectangle {
	id							: ribbonButton
	width						: (innerText.width > backgroundImage.width ? innerText.width : backgroundImage.width) + (2 * Theme.ribbonButtonPadding) // + 2*tbutton.width
	height						: Theme.ribbonButtonHeight  // backgroundImage.height + innerText.height
	radius						: 5
	color						: mice.pressed ? Theme.grayLighter : Theme.uiBackground //mice.pressed ? Theme.grayLighter : mice.containsMouse ? Theme.white : Theme.uiBackground

	//border.color				: Theme.white
	//border.width				: !mice.containsMouse ? 0 : 2

			property alias	text		: innerText.text
			property alias	source		: backgroundImage.source
			property alias	enabled		: mice.enabled
	default property var	menu
			//property int	localPadding: mice.containsMouse ? Theme.ribbonButtonPadding * 0.8 : Theme.ribbonButtonPadding

    signal clicked

	Item
	{
		anchors.centerIn:	parent
		width:				parent.width
		height:				parent.height

		scale:				mice.containsMouse && !mice.pressed ? Theme.ribbonScaleHovered : 1


		Image
		{
			id:			backgroundImage
			width:		(37 / 28) * height
			height:		Theme.ribbonButtonHeight - ( (2 * Theme.ribbonButtonPadding) + innerText.anchors.topMargin + innerText.height ) //28

			anchors.top:				parent.top
			anchors.topMargin:			Theme.ribbonButtonPadding
			anchors.horizontalCenter:	parent.horizontalCenter

			ColorOverlay {
				anchors.fill: backgroundImage
				source      : backgroundImage
				color       : mice.enabled ? "transparent" : Theme.uiBackground
			}
		}

		Text {
			id: innerText

			anchors.horizontalCenter: parent.horizontalCenter
			anchors.top             : backgroundImage.bottom
			//anchors.bottom          : parent.bottom
			anchors.topMargin       : 5

			color    : mice.enabled ? Theme.black : Theme.uiBackground
			font.bold: false
		}

		MouseArea {
			id				: mice
			anchors.fill	: parent
			hoverEnabled	: true
			acceptedButtons	: Qt.LeftButton
			onClicked		: clusterMenu.popup()

			ClusterMenu {
				id   : clusterMenu
				model: ribbonButton.menu
			}
		}
	}
}
