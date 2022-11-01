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
import JASP					1.0
import JASP.Widgets			1.0

Rectangle
{
	id				: ribbonButton
	width			: (innerText.width > _imgIndWidth ? innerText.width : _imgIndWidth) + (2 * jaspTheme.ribbonButtonPadding) // + 2*tbutton.width
	height			: jaspTheme.ribbonButtonHeight
	color			: showPressed ? jaspTheme.grayLighter : "transparent"
	z				: 1
	objectName		: "ribbonButton"

	property alias	text		: innerText.text
	property int	listIndex   : -1
	property string	source		: ""
	property bool	enabled		: true
	property bool	ready		: false
	property string moduleName	: "???"
	property string toolTip		: ""
	property var	menu		: []
	property bool	myMenuOpen	: false
	property bool	showPressed	: ribbonButton.activeFocus || myMenuOpen

	onMyMenuOpenChanged: if(!myMenuOpen) myMenuOpen = false; //Break the binding

	property real _imgIndWidth: backgroundImage.width + (menuIndicator.visible ? (menuIndicator.width + menuIndicator.anchors.leftMargin) * 2 : 0)

	signal clicked

	ToolTip.text:				ribbonButton.toolTip
	ToolTip.timeout:			jaspTheme.toolTipTimeout
	ToolTip.delay:				jaspTheme.toolTipDelay
	ToolTip.visible:			ribbonButton.toolTip !== "" && mice.containsMouse && !ribbonButton.showPressed
	
	Keys.onPressed: (event) =>
	{
		if (event.key === Qt.Key_Escape)
		{
			customMenu.hide();
		}
		else if (event.key === Qt.Key_Return || event.key === Qt.Key_Space)
		{
			if (ribbonButton.enabled && ribbonButton.ready)
			{
				if (ribbonButton.menu.rowCount() > 1) {
					ribbonButton.showMyMenu();
					event.accepted = true;
				}
			}
		}
		else if (event.key === Qt.Key_Down)
		{
			if (ribbonButton.ready && ribbonButton.enabled && ribbonButton.menu.rowCount() > 1)
				ribbonButton.showMyMenu();
			event.accepted = true;
		}
	}

	onFocusChanged:
	{
		if (!ribbonButton.focus)
		{
			myMenuOpen = false;
			customMenu.hide();
		}
	}

	function showMyMenu()
	{

		if (ribbonButton.menu.rowCount() === 0) //Probably special?
		{
			customMenu.hide()
			ribbonModel.analysisClicked("", "", "", ribbonButton.moduleName)
		}
		else if (ribbonButton.menu.rowCount() === 1)
		{
			customMenu.hide()
			ribbonModel.analysisClicked(ribbonButton.menu.getFirstAnalysisFunction(), ribbonButton.menu.getFirstAnalysisQML(), ribbonButton.menu.getFirstAnalysisTitle(), ribbonButton.moduleName)
			ribbon.focusOutRibbonBar();
		}
		else
		{
			var functionCall = function (index)
			{
				var analysisName  = customMenu.props['model'].getAnalysisFunction(index);
				var analysisTitle = customMenu.props['model'].getAnalysisTitle(index);
				var analysisQML   = customMenu.props['model'].getAnalysisQML(index);

				ribbonModel.analysisClicked(analysisName, analysisQML, analysisTitle, ribbonButton.moduleName)
				customMenu.hide();
				customMenu.focus = false;
			}

			// Key Navigation with Up or Down. Only navigate valid analysis items
			//	@index
			//	@direction: +1 or -1
			var navigateFunc = function (index, direction)
			{
				let nextIndex = mod(index + direction, customMenu.props['model'].rowCount());
				while(true)
				{
					let name	  = customMenu.props['model'].getAnalysisFunction(nextIndex);
					let isEnabled = customMenu.props['model'].isAnalysisEnabled(nextIndex);

					if (name !== "" && name !== '???' && isEnabled)
						break;

					nextIndex = mod(nextIndex + direction, customMenu.props['model'].rowCount());
				}
				return nextIndex;
			}

			var props =
			{
				"model"			: ribbonButton.menu,
				"functionCall"	: functionCall,
				"hasIcons"		: ribbonButton.menu.hasIcons(),
				"navigateFunc"	: navigateFunc
			};

			customMenu.toggle(ribbonButton, props, 0, ribbonButton.height);

			myMenuOpen = Qt.binding(function() { return customMenu.visible && customMenu.sourceItem === ribbonButton; });
		}
	}

	Rectangle
	{
		id		: borderLeft
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
		id		: borderRight
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
			visible:	ribbonButton.ready
			source:		ribbonButton.source === "" ? jaspTheme.iconPath + "error.png" : ribbonButton.source

			anchors
			{
				top				: parent.top
				topMargin		: jaspTheme.ribbonButtonPadding
				horizontalCenter: parent.horizontalCenter
			}

		}

		LoadingIndicator
		{
			anchors.top:		 backgroundImage.top
			anchors.left:		 backgroundImage.left
			width:				 backgroundImage.width
			height:				 backgroundImage.height
			visible:			!ribbonButton.ready
			z:					 backgroundImage.z + 1
		}

		Image
		{
			id:					menuIndicator

			anchors.left:		backgroundImage.right
			anchors.leftMargin: 5   * preferencesModel.uiScale
			height:				0.3 * backgroundImage.height
			width:				visible ? height : 0
			anchors.top:		backgroundImage.top
			source:				jaspTheme.iconPath + "/toolbutton-menu-indicator.svg"
			opacity:			ribbonButton.enabled ? 1 : 0.5
			visible:			ribbonButton.menu && ribbonButton.menu.rowCount() > 1
		}

		Text
		{
			id							: innerText

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

			onClicked: (mouse)=>
			{
				if(!ribbonButton.ready) return; //Be patient!

				if (myMenuOpen)
				{
					ribbonButton.focus = false;
					customMenu.hide();
				}
				else
				{
					fileMenuModel.visible	= false;
					modulesMenu.opened		= false;
					ribbon.focusOutFileMenu();
					ribbon.focusOutModules();
					ribbon.focusOutPreviousRibbonButton();
					ribbon.goToRibbonIndex(ribbonButton.listIndex);
					ribbonButton.showMyMenu();
				}
				mouse.accepted = false;
			}
		}
	}
}
