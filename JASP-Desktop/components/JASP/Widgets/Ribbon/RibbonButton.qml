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
	id				: ribbonButton
	width			: (innerText.width > _imgIndWidth ? innerText.width : _imgIndWidth) + (2 * Theme.ribbonButtonPadding) // + 2*tbutton.width
	height			: Theme.ribbonButtonHeight  // backgroundImage.height + innerText.height
	color			: showPressed ? Theme.grayLighter : "transparent"
	border.color	: myMenuOpen ? Theme.grayDarker : Theme.gray
	border.width	: showPressed ? 1 : 0
	z				: 1
	//radius			: 4

	property alias	text		: innerText.text
	property alias	source		: backgroundImage.source
	property bool	enabled		: true
	property string moduleName	: "???"
	property var	menu		: []
	property bool	myMenuOpen	: false
	property bool	showPressed	: myMenuOpen || mice.pressed

	onMyMenuOpenChanged: if(!myMenuOpen) myMenuOpen = false; //Break the binding

	property real _imgIndWidth: backgroundImage.width + (menuIndicator.visible ? (menuIndicator.width + menuIndicator.anchors.leftMargin) * 2 : 0)

	signal clicked

	Item
	{
		anchors.centerIn	: parent
		width				: parent.width
		height				: parent.height
		scale				: mice.containsMouse && !ribbonButton.showPressed ? Theme.ribbonScaleHovered : 1

		Image
		{
			id:			backgroundImage
			z:			1
			width:		(37 / 28) * height
			height:		Theme.ribbonButtonHeight - ( (2 * Theme.ribbonButtonPadding) + innerText.anchors.topMargin + innerText.height ) //28
			opacity:	ribbonButton.enabled ? 1 : 0.5
			smooth:		true
			mipmap:		true
			fillMode:	Image.PreserveAspectFit

			anchors
			{
				top				: parent.top
				topMargin		: Theme.ribbonButtonPadding
				horizontalCenter: parent.horizontalCenter
			}

		}

		Image
		{
			id: menuIndicator

			anchors.left:		backgroundImage.right
			anchors.leftMargin: 5   * preferencesModel.uiScale
			height:				0.3 * backgroundImage.height
			width:				visible ? height : 0
			anchors.top:		backgroundImage.top
			source:				"qrc:/icons/toolbutton-menu-indicator.svg"
			opacity:			ribbonButton.enabled ? 1 : 0.5
			visible:			ribbonButton.menu ? ribbonButton.menu.rowCount() > 1 : false
		}

		Text
		{
			id	: innerText

			anchors.horizontalCenter	: backgroundImage.horizontalCenter
			anchors.top					: backgroundImage.bottom
			anchors.topMargin			: 5 * preferencesModel.uiScale
			color						: ribbonButton.enabled ? Theme.black : Theme.gray
			font						: Theme.fontRibbon
			renderType					: Text.QtRendering //Because this might be transformed and ugly if done natively
		}

		MouseArea
		{
			id				: mice
			anchors.fill	: parent
			hoverEnabled	: true
			acceptedButtons	: Qt.LeftButton
			cursorShape		: Qt.PointingHandCursor
			enabled			: ribbonButton.enabled

			onClicked:
			{
				fileMenuModel.visible	= false;
				modulesMenu.opened		= false;
				mouse.accepted			= false;
				
				customMenu.remove()

				if (ribbonButton.menu.rowCount() === 1)
					ribbonModel.analysisClickedSignal(ribbonButton.menu.getFirstAnalysisFunction(), ribbonButton.menu.getFirstAnalysisTitle(), ribbonButton.moduleName)
				else
				{
					var functionCall = function (index)
					{
						var analysisName  = customMenu.props['model'].getAnalysisFunction(index);
						var analysisTitle = customMenu.props['model'].getAnalysisTitle(index);
						ribbonModel.analysisClickedSignal(analysisName, analysisTitle, ribbonButton.moduleName)
						customMenu.remove();
					}

					var props = {
						"model"			: ribbonButton.menu,
						"functionCall"	: functionCall,
						"hasIcons"		: ribbonButton.menu.hasIcons()
					};

					customMenu.showMenu(ribbonButton, props, 1 , ribbonButton.height);

					myMenuOpen = Qt.binding(function() { return customMenu.visible; });
				}
			}
		}
	}
}
