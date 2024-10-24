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
import JASP
import QtQuick.Controls	2.12

Window
{
	id:					mainWindowRoot
	title:				mainWindow.windowTitle
	visible:			true
	width:				1280
	height:				720
	flags:				Qt.Window | Qt.WindowFullscreenButtonHint
	color:				jaspTheme.white
	minimumWidth:		jaspTheme.formWidth + 2 * jaspTheme.splitHandleWidth + jaspTheme.scrollbarBoxWidthBig + 3
	minimumHeight:		400 * jaspTheme.uiScale

	onVisibleChanged:
		if(!visible)
		{
			helpModel.visible = false;
			aboutModel.visible = false;
		}

	property real devicePixelRatio: Screen.devicePixelRatio

	onDevicePixelRatioChanged: if(devicePixelRatio > 0) mainWindow.screenPPI = devicePixelRatio * 96

	onClosing: (close)=>
	{
		close.accepted = mainWindow.checkPackageModifiedBeforeClosing();

		if(close.accepted)
			mainWindow.closeWindows();
	}

	function toggleFullScreen()
	{
		mainWindowRoot.visibility = mainWindowRoot.visibility === Window.FullScreen ? Window.Windowed : Window.FullScreen;
	}

	function changeFocusToRibbon()
	{
		ribbon.focus = true;
 		ribbon.focusOnRibbonMenu();
	}

	function showWorkspaceMenu()
	{
		ribbonModel.showData()
		fileMenuModel.visible = false
		modulesMenu.opened	= true
	}

	function changeFocusToModulesMenu()
	{
		ribbon.showModulesMenuPressed();
	}

	function changeFocusToFileMenu()
	{
		ribbon.forceActiveFocus();
		ribbon.showFileMenuPressed();
	}

	function mod (a, n)
	{
		return (a + n) % n;
	}

	Item
	{
		anchors.fill:	parent

		Shortcut { onActivated: mainWindow.showEnginesWindow();					sequences: ["Ctrl+Alt+Shift+E"];								context: Qt.ApplicationShortcut; }
		Shortcut { onActivated: mainWindow.saveKeyPressed();					sequences: ["Ctrl+S", Qt.Key_Save];								context: Qt.ApplicationShortcut; }
		Shortcut { onActivated: mainWindow.saveAsKeyPressed();					sequences: ["Ctrl+Shift+S", Qt.Key_SaveAs];						context: Qt.ApplicationShortcut; }
		Shortcut { onActivated: { ribbon.showFileMenuPressed(); mainWindow.openKeyPressed();}
																				sequences: ["Ctrl+O"];											context: Qt.ApplicationShortcut; }
		//This is now redo! Shortcut { onActivated: mainWindow.syncKeyPressed();					sequences: ["Ctrl+Y", Qt.Key_Reload];							context: Qt.ApplicationShortcut; }
		Shortcut { onActivated: mainWindow.zoomInKeyPressed();					sequences: [Qt.Key_ZoomIn, "Ctrl+Plus", "Ctrl+\+", "Ctrl+\="];	context: Qt.ApplicationShortcut; }
		Shortcut { onActivated: mainWindow.zoomOutKeyPressed();					sequences: [Qt.Key_ZoomOut, "Ctrl+Minus", "Ctrl+\-"];			context: Qt.ApplicationShortcut; }
		Shortcut { onActivated: mainWindow.zoomResetKeyPressed();				sequences: ["Ctrl+0", Qt.Key_Zoom];								context: Qt.ApplicationShortcut; }
		Shortcut { onActivated: mainWindow.refreshKeyPressed();					sequences: ["Ctrl+R", Qt.Key_Refresh];							context: Qt.ApplicationShortcut; }
		Shortcut { onActivated: mainWindowRoot.close();							sequences: ["Ctrl+Q", Qt.Key_Close];							context: Qt.ApplicationShortcut; }
		Shortcut { onActivated: fileMenuModel.close();							sequences: ["Ctrl+W"];											}
		Shortcut { onActivated: mainWindowRoot.toggleFullScreen();				sequences: ["Ctrl+M", Qt.Key_F11];								context: Qt.ApplicationShortcut; }
		Shortcut { onActivated: mainWindowRoot.changeFocusToFileMenu();			sequences: ["Home",   Qt.Key_Home, Qt.Key_Menu];				}
		Shortcut { onActivated: mainWindow.setLanguage(0);						sequences: ["Ctrl+1"];											context: Qt.ApplicationShortcut; }
		Shortcut { onActivated: mainWindow.setLanguage(1);						sequences: ["Ctrl+2"];											context: Qt.ApplicationShortcut; }
		Shortcut { onActivated: mainWindow.undo();								sequences: ["Ctrl+Z", Qt.Key_Undo];								context: Qt.ApplicationShortcut; }
		Shortcut { onActivated: mainWindow.redo();								sequences: ["Ctrl+Shift+Z", "Ctrl-Y", Qt.Key_Redo];				context: Qt.ApplicationShortcut; }

        Shortcut { onActivated: { dynamicModules.refreshDeveloperModule(false, true);}  sequences: ["Ctrl+Shift+U"];                     context: Qt.ApplicationShortcut; }
        Shortcut { onActivated: { dynamicModules.refreshDeveloperModule(true, false);}  sequences: ["Ctrl+Shift+R"];                     context: Qt.ApplicationShortcut; }
        Shortcut { onActivated: { dynamicModules.refreshDeveloperModule(true, true);}   sequences: ["Ctrl+Shift+D"];                     context: Qt.ApplicationShortcut; }


		RibbonBar
		{
			id:		ribbon
			z:		6
			focus:	true

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
					enabled:		preferencesModel.animationsOn

					PropertyAnimation
					{
						id:				darkeningBackgroundRectDarkening
						duration:		jaspTheme.fileMenuSlideDuration
						easing.type:	Easing.OutCubic
					}
				}
			}

			onPressed: (mouse)=>
			{
				if(customMenu.visible)
				{
					customMenu.hide()
					mouse.accepted = false;
				}

				fileMenuModel.visible	= false
				modulesMenu.opened		= false
				ribbon.focusOutRibbonBar();
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
		ResizeDataDialog			{ id: resizeDataDialog		}
		RenameColumnDialog			{ id: renameColumnDialog	}
		PlotEditor					{ id: plotEditingDialog		}

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

	UIScaleNotifier { anchors.centerIn:	parent }

	ProgressBarHolder
	{
		visible:			mainWindow.progressBarVisible
		z:					10
		anchors.fill:		parent
	}
	
	//Utility:
	property Item _toolTipOverrideItem: Item
	{
		//These properties override those for ALL attached ToolTips in the application
		//ToolTip.toolTip shouldn't be changed anywhere else otherwise we get hard to debug behaviour
		ToolTip.toolTip.background:		Rectangle { color: jaspTheme.tooltipBackgroundColor }
		ToolTip.toolTip.contentItem:	Text
		{
			font:			jaspTheme.font
			wrapMode:		Text.WrapAtWordBoundaryOrAnywhere
			text:			ToolTip.toolTip.text
			color:			jaspTheme.textEnabled
		}
		ToolTip.toolTip.implicitWidth:			Math.min(jaspTheme.formWidth * 0.8, ToolTip.toolTip.contentItem.implicitWidth + (2 * ToolTip.toolTip.padding))
		ToolTip.toolTip.z:						1234
	}
}
