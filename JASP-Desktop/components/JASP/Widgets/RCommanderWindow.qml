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

import QtQuick			2.12
import QtQuick.Window	2.12
import QtQuick.Controls 2.12
import JASP				1.0
import JASP.Widgets		1.0 as JW
import JASP.Controls	1.0 as JC

Window
{
    id:					rcmdRoot
	title:				"R in JASP"
	visible:			true
	width:				1248
	height:				768
	flags:				Qt.Window | Qt.WindowFullscreenButtonHint
	color:				jaspTheme.white

	minimumWidth:		800 * preferencesModel.uiScale
	minimumHeight:		600 * preferencesModel.uiScale

	function toggleFullScreen()
	{
	    rcmdRoot.visibility = rcmdRoot.visibility === Window.FullScreen ? Window.Windowed : Window.FullScreen;
	}

	RCommander
	{
			id:				rCmd
			onScrollDown:	outputScroll.contentY = outputScroll.contentHeight - outputScroll.height;
			onCloseWindow:	rcmdRoot.close()
	}

	//A SplitView here could be nice
	Item
	{

		anchors.fill:		parent
		//anchors.margins:	jaspTheme.generalAnchorMargin

        Shortcut { onActivated: mainWindow.zoomInKeyPressed();					sequences: [Qt.Key_ZoomIn, "Ctrl+Plus", "Ctrl+\+", "Ctrl+\="];	}
		Shortcut { onActivated: mainWindow.zoomOutKeyPressed();					sequences: [Qt.Key_ZoomOut, "Ctrl+Minus", "Ctrl+\-"];			}
		Shortcut { onActivated: mainWindow.zoomResetKeyPressed();				sequences: ["Ctrl+0", Qt.Key_Zoom];								}
		Shortcut { onActivated: mainWindow.close();								sequences: ["Ctrl+Q", Qt.Key_Close];							}
		Shortcut { onActivated: rcmdRoot.close();								sequences: ["Ctrl+W"];											}
		Shortcut { onActivated: rcmdRoot.toggleFullScreen();					sequences: ["Ctrl+M", Qt.Key_F11];								}

		Rectangle
		{
			id:	outputContainer

			anchors
			{
				top:		parent.top
				left:		parent.left
				right:		parent.right
				bottom:		codeEntryContainer.top
				margins:	jaspTheme.generalAnchorMargin
			}


			border.color:	jaspTheme.uiBorder
			color:			jaspTheme.white

			Flickable
			{
				id:						outputScroll
				clip:					true
				contentHeight:			outputWindow.implicitHeight
				contentWidth:			width
				flickableDirection:		Flickable.VerticalFlick
				interactive:			false

				onContentYChanged:		if(contentHeight < height) rCmd.countDownToScroll();

				anchors
				{
					top:	parent.top
					left:	parent.left
					right:	vertScroll.left
					bottom:	parent.bottom
				}

				TextArea.flickable: TextArea
				{
					id:					outputWindow
					text:				rCmd.output
					font:				jaspTheme.fontConsole
					width:				outputScroll.width
					wrapMode:			TextEdit.Wrap
					color:				jaspTheme.textEnabled
					selectedTextColor:	jaspTheme.textDisabled
					selectionColor:		jaspTheme.black
					selectByMouse:		true


					//textFormat:			TextEdit.RichText //too much trouble
				}

				MouseArea
				{
					z:					-1
					acceptedButtons:	Qt.NoButton
					anchors.fill:		parent
					onWheel:			vertScroll.scrollWheel(wheel)
				}
			}

			JC.JASPScrollBar
			{
				id:					vertScroll
				flickable:			outputScroll
				manualAnchor:		true

				anchors
				{
					top:	parent.top
					right:	parent.right
					bottom:	parent.bottom
				}
			}
		}

		Item
		{
		    id:		codeEntryContainer
			height: 100;// Math.min(Math.max(codeEntry.implicitHeight, 20 * jaspTheme.uiScale), 200 * jaspTheme.uiScale)
			anchors
			{
			    left:	parent.left
				right:	parent.right
				bottom:	parent.bottom
			}

			Rectangle
			{
				border.color:	jaspTheme.uiBorder
				color:			jaspTheme.white

				//ScrollView
				Flickable
				{
					id:					codeEntryFlickable

					TextArea.flickable: TextArea
					{
						id:						codeEntry
						font:					jaspTheme.fontConsole
						color:					jaspTheme.textEnabled
						selectedTextColor:		jaspTheme.textDisabled
						selectionColor:			jaspTheme.black
						selectByMouse:			true
						wrapMode:				TextEdit.Wrap

						placeholderText:		mainWindow.dataAvailable ? qsTr("Enter you R code here.\nThe data is fully available as 'jaspData' and filtered as 'jaspFiltered'.") : qsTr("Enter you R code here.")
						placeholderTextColor:	jaspTheme.grayDarker

						Shortcut { onActivated: runButton.runCode();	sequences: ["Ctrl+Enter", "Ctrl+Return", Qt.Key_F5];}
						Shortcut { onActivated: codeEntry.undo();		sequences: ["Ctrl+Z", ];}
						Shortcut { onActivated: codeEntry.selectAll();	sequences: ["Ctrl+A", ];}
					}

					Keys.onUpPressed: codeEntry.text = rCmd.lastCmd

					anchors
					{
						top:		parent.top
						left:		parent.left
						right:		codeEntryScrollbar.left
						bottom:		parent.bottom

					}

					interactive:			false


					MouseArea
					{
						z:					-1
						acceptedButtons:	Qt.NoButton
						anchors.fill:		parent
						onWheel:			codeEntryScrollbar.scrollWheel(wheel)
					}
				}

				JC.JASPScrollBar
				{
					id:					codeEntryScrollbar
					flickable:			codeEntryFlickable;
					manualAnchor:		true

					anchors
					{
						top:		parent.top
						right:		parent.right
						bottom:		parent.bottom
					}
				}

				anchors
				{
					top:		parent.top
					left:		runButton.right
					right:		parent.right
					bottom:		parent.bottom
					margins:	jaspTheme.generalAnchorMargin
				}
			}

			JW.RectangularButton
			 {
				 id:		clearOutput
				 text:		qsTr("Clear Output")
				 onClicked:	rCmd.output = "Cleared...";
				 width:		Math.max(clearOutput.implicitWidth, runButton.implicitWidth)

				 anchors
				 {
					 top:			parent.top
					 left:			parent.left
					 bottom:		parent.verticalCenter
					 margins:		jaspTheme.generalAnchorMargin
					 bottomMargin:	jaspTheme.generalAnchorMargin * 0.5
				 }
			 }

		   JW.RectangularButton
			{
			    id:			runButton
				text:		qsTr("Run Code")
				onClicked:	runCode();
				width:		clearOutput.width

				toolTip:	qsTr("Pressing Ctrl+Enter or F5 will also run the code")

				function runCode() { if(rCmd.runCode(codeEntry.text)) codeEntry.text = ""; }

				anchors
				{
					top:		parent.verticalCenter
					left:		parent.left
					bottom:		parent.bottom
					margins:	jaspTheme.generalAnchorMargin
					topMargin:	jaspTheme.generalAnchorMargin * 0.5
				}
			}
		}
	}
}
