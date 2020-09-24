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

import QtQuick				2.11
import QtQuick.Controls		2.4
import QtGraphicalEffects	1.12
import JASP					1.0

Rectangle
{
	id				: ribbonButton
	width			: (innerText.width > _imgIndWidth ? innerText.width : _imgIndWidth) + (2 * jaspTheme.ribbonButtonPadding) // + 2*tbutton.width
	height			: jaspTheme.ribbonButtonHeight
	color			: showPressed ? jaspTheme.grayLighter : "transparent"
	z				: 1
	objectName      : "ribbonButton"

	property alias	text		: innerText.text
	property alias	source		: backgroundImage.source
	property bool	enabled		: true
	property string moduleName	: "???"
	property string toolTip		: ""
	property var	menu		: []
	property bool	myMenuOpen	: false
	property bool	showPressed	: myMenuOpen || mice.pressed

	onMyMenuOpenChanged: if(!myMenuOpen) myMenuOpen = false; //Break the binding

	property real _imgIndWidth: backgroundImage.width + (menuIndicator.visible ? (menuIndicator.width + menuIndicator.anchors.leftMargin) * 2 : 0)

	signal clicked

	ToolTip.text:				toolTip
	ToolTip.timeout:			jaspTheme.toolTipTimeout
	ToolTip.delay:				jaspTheme.toolTipDelay
	ToolTip.visible:			toolTip !== "" && mice.containsMouse

    Rectangle
    {
        id      : borderLeft
        width   : showPressed ? 1 : 0
		color   : myMenuOpen  ? jaspTheme.grayDarker  : jaspTheme.gray
        anchors
        {
            left	: parent.left
            top		: parent.top
            bottom	: parent.bottom
        }
    }

    Rectangle
    {
        id      : borderRight
        width   : showPressed ? 1 : 0
		color   : myMenuOpen  ? jaspTheme.grayDarker  : jaspTheme.gray
        anchors
        {
            right	: parent.right
            top		: parent.top
            bottom	: parent.bottom
        }
    }

	Item
	{
		anchors.centerIn	: parent
		width				: parent.width
		height				: parent.height
		scale				: mice.containsMouse && !ribbonButton.showPressed ? jaspTheme.ribbonScaleHovered : 1

		Image
		{
			id:			backgroundImage
			z:			1
			width:		(37 / 28) * height
			height:		jaspTheme.ribbonButtonHeight - ( (2 * jaspTheme.ribbonButtonPadding) + innerText.anchors.topMargin + innerText.height ) //28
			opacity:	ribbonButton.enabled ? 1 : 0.5
			smooth:		true
			mipmap:		true
			fillMode:	Image.PreserveAspectFit

			anchors
			{
				top				: parent.top
				topMargin		: jaspTheme.ribbonButtonPadding
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
			source:				jaspTheme.iconPath + "/toolbutton-menu-indicator.svg"
			opacity:			ribbonButton.enabled ? 1 : 0.5
			visible:			ribbonButton.menu ? ribbonButton.menu.rowCount() > 1 : false
		}

		Text
		{
			id	: innerText

			anchors.horizontalCenter	: backgroundImage.horizontalCenter
			anchors.top					: backgroundImage.bottom
			anchors.topMargin			: 5 * preferencesModel.uiScale
			color						: ribbonButton.enabled ? jaspTheme.black : jaspTheme.gray
			font						: jaspTheme.fontRibbon
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
				
				if (ribbonButton.menu.rowCount() === 1)
				{
					customMenu.hide()
					ribbonModel.analysisClicked(ribbonButton.menu.getFirstAnalysisFunction(), ribbonButton.menu.getFirstAnalysisQML(), ribbonButton.menu.getFirstAnalysisTitle(), ribbonButton.moduleName)
				}
				else
				{
					var functionCall = function (index)
					{
						var analysisName  = customMenu.props['model'].getAnalysisFunction(index);
						var analysisTitle = customMenu.props['model'].getAnalysisTitle(index);
						var analysisQML = customMenu.props['model'].getAnalysisQML(index);
						ribbonModel.analysisClicked(analysisName, analysisQML, analysisTitle, ribbonButton.moduleName)
						customMenu.hide();
					}

					var props = {
						"model"			: ribbonButton.menu,
						"functionCall"	: functionCall,
						"hasIcons"		: ribbonButton.menu.hasIcons()
					};

					customMenu.toggle(ribbonButton, props, 0 , ribbonButton.height);

					myMenuOpen = Qt.binding(function() { return customMenu.visible && customMenu.sourceItem === ribbonButton; });
				}
			}
		}
	}
}
