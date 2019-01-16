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

Rectangle
{
	id							: ribbonButton
	width						: (innerText.width > backgroundImage.width ? innerText.width : backgroundImage.width) + (2 * Theme.ribbonButtonPadding) // + 2*tbutton.width
	height						: Theme.ribbonButtonHeight  // backgroundImage.height + innerText.height
	radius						: 5
	color						: mice.pressed ? Theme.grayLighter : "transparent"

	//border.color				: Theme.white
	//border.width				: !mice.containsMouse ? 0 : 2

			property alias	text		: innerText.text
			property alias	source		: backgroundImage.source
			property bool	enabled		: true
			property string moduleName	: "???"
			property string ribbonTitle : "???"
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
			z:			1
			width:		(37 / 28) * height
			height:		Theme.ribbonButtonHeight - ( (2 * Theme.ribbonButtonPadding) + innerText.anchors.topMargin + innerText.height ) //28

			anchors.top:				parent.top
			anchors.topMargin:			Theme.ribbonButtonPadding
			anchors.horizontalCenter:	parent.horizontalCenter

			ColorOverlay
			{
				anchors.fill: backgroundImage
				source      : backgroundImage
				color       : Theme.gray
				visible		: !ribbonButton.enabled
			}
		}

		Text
		{
			id: innerText

			anchors.horizontalCenter:	parent.horizontalCenter
			anchors.top:				backgroundImage.bottom
			anchors.topMargin:			5
			color:						ribbonButton.enabled ? Theme.black : Theme.gray
			font:						Theme.font
		}

		MouseArea
		{
			id				: mice
			anchors.fill	: parent
			hoverEnabled	: true
			acceptedButtons	: Qt.LeftButton
			onClicked		: menuLoader.sourceComponent = menuComp
			cursorShape		: Qt.PointingHandCursor
			enabled			: ribbonButton.enabled

			Loader
			{
				id:					menuLoader
				sourceComponent:	null
			}

			Component
			{
				id: menuComp

				ClusterMenu {
					id   : clusterMenu
					model: ribbonButton.menu
					posX : innerText.x
					posY : ribbonButton.y + (ribbonButton.height)

					moduleName:				ribbonButton.moduleName
					ribbonTitle:			ribbonButton.ribbonTitle
					Component.onCompleted:	clusterMenu.open()
					onClosed:				menuLoader.sourceComponent = null
				}
			}

			// Rectangle {
			// 	// bottom shadow
			// 	width  : clusterMenu.width
			// 	height : 5
			//
			// 	visible: clusterMenu.opened
			//
			// 	x : clusterMenu.x
			// 	y : clusterMenu.y + clusterMenu.height
			// 	z : clusterMenu.z
			//
			// 	gradient:	Gradient {
			// 		GradientStop { position: 0.0; color: Theme.shadow }
			// 		GradientStop { position: 1.0; color: "transparent" }
			// 	}
			// }
		}
	}
}
