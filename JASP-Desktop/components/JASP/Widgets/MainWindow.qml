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

import QtQuick			2.11
import QtQuick.Window	2.11
import JASP.Widgets		1.0

Window
{
	id:					mainWindowRoot
	title:				mainWindow.windowTitle
	visible:			true
	width:				1248
	height:				768
	flags:				Qt.Window | Qt.WindowFullscreenButtonHint
	color:				jaspTheme.white

	minimumWidth:		800 * preferencesModel.uiScale
	minimumHeight:		600 * preferencesModel.uiScale

	onVisibleChanged:
		if(!visible)
		{
			helpModel.visible = false;
			aboutModel.visible = false;
		}

	property real devicePixelRatio: Screen.devicePixelRatio

	onDevicePixelRatioChanged: if(devicePixelRatio > 0) mainWindow.screenPPI = devicePixelRatio * 96

	onClosing:
	{
		close.accepted = mainWindow.checkPackageModifiedBeforeClosing();

		if(close.accepted)
		{
			aboutModel.visible = false;
			helpModel.visible  = false;
		}
	}

	function toggleFullScreen()
	{
		mainWindowRoot.visibility = mainWindowRoot.visibility === Window.FullScreen ? Window.Windowed : Window.FullScreen;
	}

	Item
	{
		anchors.fill:	parent
		focus:			true

		Shortcut { onActivated: mainWindow.saveKeyPressed();		sequences: ["Ctrl+S"];											}
		Shortcut { onActivated: mainWindow.openKeyPressed();		sequences: ["Ctrl+O"];											}
		Shortcut { onActivated: mainWindow.syncKeyPressed();		sequences: ["Ctrl+Y"];											}
		Shortcut { onActivated: mainWindow.zoomInKeyPressed();		sequences: [Qt.Key_ZoomIn, "Ctrl+Plus", "Ctrl+\+", "Ctrl+\="];	}
		Shortcut { onActivated: mainWindow.zoomOutKeyPressed();		sequences: [Qt.Key_ZoomOut, "Ctrl+Minus", "Ctrl+\-"];			}
		Shortcut { onActivated: mainWindow.refreshKeyPressed();		sequences: ["Ctrl+R"];											}
		Shortcut { onActivated: mainWindow.zoomResetKeyPressed();	sequences: ["Ctrl+0"];											}
		Shortcut { onActivated: mainWindowRoot.close();				sequences: ["Ctrl+Q", Qt.Key_Close];							}
		Shortcut { onActivated: mainWindowRoot.toggleFullScreen();	sequences: ["Ctrl+M"];											}

		RibbonBar
		{
			id	: ribbon
			z	: 6

			anchors
			{
				top:	parent.top
				left:	parent.left
				right:	parent.right
			}
		}

		CustomMenu
		{
			id:			customMenu
			z:			5
		}

		FileMenu
		{
			id:			filemenu
			z:			3

			anchors
			{
				top:	ribbon.bottom
				left:	parent.left
				bottom:	parent.bottom
			}
		}

		WelcomePage
		{
			id:			welcomePage
			z:			0
			visible:	mainWindow.welcomePageVisible

			anchors
			{
				top:	ribbon.bottom
				left:	parent.left
				right:	parent.right
				bottom:	parent.bottom
			}
		}

		MainPage
		{
			id:			mainpage
			z:			0
			visible:	!mainWindow.welcomePageVisible

			anchors
			{
				top:	ribbon.bottom
				left:	parent.left
				right:	parent.right
				bottom:	parent.bottom
			}
		}



		MouseArea
		{
			//visible:					enabled
			enabled: 					fileMenuModel.visible || modulesMenu.opened || customMenu.visible
			z:							enabled ? 1 : -5
			hoverEnabled:				true
			onContainsMouseChanged:		if(containsMouse) ribbonModel.highlightedModuleIndex = -1
			anchors.fill:				parent
			propagateComposedEvents:	true

			Rectangle
			{
				id:				darkeningBackgroundRect;
				color:			jaspTheme.darkeningColour
				anchors.fill:	parent;
				opacity:		visible ? 0.4 : 0.0
				visible:		fileMenuModel.visible || modulesMenu.opened

				Behavior on opacity
				{
					enabled:		!preferencesModel.safeGraphics

					PropertyAnimation
					{
						id:				darkeningBackgroundRectDarkening
						duration:		jaspTheme.fileMenuSlideDuration
						easing.type:	Easing.OutCubic
					}
				}
			}

			onPressed:
			{
				if(customMenu.visible)
				{
					customMenu.hide()
					mouse.accepted = false;
				}

				fileMenuModel.visible	= false
				modulesMenu.opened		= false
			}
		}

		ModulesMenu
		{
			id:			modulesMenu
			z:			2

			anchors
			{
				top:	ribbon.bottom
				right:	parent.right
				bottom:	parent.bottom
			}
		}

		CreateComputeColumnDialog	{ id: createComputeDialog	}
		ModuleInstaller				{ id: moduleInstallerDialog	}
//		PlotEditor					{ id: plotEditingDialog		}
		Loader
		{
			id: plotEditingDialog
			width: parent.width
			height: parent.height
			source: plotEditorFile

			onSourceChanged: console.log("PlotEditor QML file changed")
		}

		/*MessageBox
		{
			id:	msgBox
			z:	2

			Connections
			{
				target:			mainWindow
				onShowWarning:	msgBox.showWarning(title, message)
			}
		}*/
	}

	UIScaleNotifier
	{
		anchors.centerIn:	parent
	}

	ProgressBarHolder
	{
		visible:			mainWindow.progressBarVisible
		z:					10
		anchors.fill:		parent
	}
}
