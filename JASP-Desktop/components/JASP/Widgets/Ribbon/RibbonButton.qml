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
import QtGraphicalEffects 1.12
import JASP.Theme 1.0

Rectangle
{
	id							: ribbonButton
	width						: (innerText.width > backgroundImage.width ? innerText.width : backgroundImage.width) + (2 * Theme.ribbonButtonPadding) // + 2*tbutton.width
	height						: Theme.ribbonButtonHeight  // backgroundImage.height + innerText.height
	//radius						: 5
	color						: mice.pressed ? Theme.grayLighter : "transparent"

	//border.color				: Theme.white
	//border.width				: !mice.containsMouse ? 0 : 2

			property alias	text		: innerText.text
			property alias	source		: backgroundImage.source
			property bool	enabled		: true
			property string moduleName	: "???"
			property string moduleTitle : "???"
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
			visible:	ribbonButton.enabled

			anchors.top:				parent.top
			anchors.topMargin:			Theme.ribbonButtonPadding
			anchors.horizontalCenter:	parent.horizontalCenter
		}

		Desaturate
		{
			z:				2
			anchors.fill:	backgroundImage
			source:			backgroundImage
			visible:		!ribbonButton.enabled
			desaturation:	0.95
		}

		Text
		{
			id: innerText

			anchors.horizontalCenter:	parent.horizontalCenter
			anchors.top:				backgroundImage.bottom
			anchors.topMargin:			5 * preferencesModel.uiScale
			color:						ribbonButton.enabled ? Theme.black : Theme.gray
			font:						Theme.font
		}

		MouseArea
		{
			id				: mice
			anchors.fill	: parent
			hoverEnabled	: true
			acceptedButtons	: Qt.LeftButton
			cursorShape		: Qt.PointingHandCursor
			enabled			: ribbonButton.enabled

			onClicked		:
			{
				//It would be nice to check here if the menu only has one entry, if so just open that instead of the menu

				if(fileMenuModel.visible) fileMenuModel.visible = false
				if(modulesMenu.opened)		modulesMenu.opened  = false

				if(ribbonButton.menu.rowCount() === 1)
					ribbonModel.analysisClickedSignal(ribbonButton.menu.getFirstAnalysisEntry(), ribbonButton.ribbonTitle, ribbonButton.moduleName)
				else
					menuLoader.sourceComponent = menuComp
			}

			Loader
			{
				id:					menuLoader
				sourceComponent:	null
			}

			Component
			{
				id: menuComp

				ClusterMenu
				{
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

			Rectangle {
				id		: borderRect
				width	: clusterMenu.width
				height	: clusterMenu.height
				x		: innerText.x
				y		: ribbonButton.y + (ribbonButton.height)
				z		: clusterMenu.z + 1

				// visible	: clusterMenu.opened

				border.width: 1
				border.color: "black"
			}

			// RectangularGlow
			// {
			// 	anchors.fill: clusterMenu
			// 	glowRadius	: 5
			// 	spread		: 0.2
			// 	color		: Theme.grayDarker
			// 	cornerRadius: clusterMenu.radius + glowRadius
			// 	z			: clusterMenu.z - 1
			// 	visible		: clusterMenu.opened
			// }
		}
	}

	Rectangle
	{
		anchors.top:				parent.bottom
		anchors.horizontalCenter:	parent.horizontalCenter
		border.color:				Theme.uiBorder
		border.width:				1
		color:						Theme.uiBackground
		visible:					mice.containsMouse && !mice.pressed
		height:						moduleNameText.implicitHeight + ( 2 * Theme.ribbonButtonPadding)
		width:						moduleNameText.implicitWidth  + ( 2 * Theme.ribbonButtonPadding)

		Text
		{
			id:						moduleNameText
			anchors.centerIn:		parent
			font:					Theme.fontLabel
			text:					ribbonButton.moduleTitle
		}
	}

}
