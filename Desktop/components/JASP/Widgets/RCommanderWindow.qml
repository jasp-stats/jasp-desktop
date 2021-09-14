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
	title:				qsTr("R in JASP")
	visible:			true
	width:				600 * preferencesModel.uiScale
	height:				300 * preferencesModel.uiScale
	flags:				Qt.Window | Qt.WindowFullscreenButtonHint
	color:				jaspTheme.white

	minimumWidth:		300 * preferencesModel.uiScale
	minimumHeight:		200 * preferencesModel.uiScale

	function toggleFullScreen()
	{
	    rcmdRoot.visibility = rcmdRoot.visibility === Window.FullScreen ? Window.Windowed : Window.FullScreen;
	}

	RCommander
	{
			id:				rCmd
			output:			qsTr("Welcome to R in JASP!");
			onScrollDown:	outputScroll.contentY = outputScroll.contentHeight - outputScroll.height;
			onCloseWindow:	rcmdRoot.close()
			onActivated:
			{
				rcmdRoot.visible = false;
				rcmdRoot.visible = true;
			}
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
			clip:			true

			Flickable
			{
				id:						outputScroll

				contentHeight:			bottomFeeder.height
				contentWidth:			width
				flickableDirection:		Flickable.VerticalFlick
				interactive:			false
				onContentYChanged:		if(contentHeight < height) rCmd.countDownToScroll();

				anchors
				{
					top:		parent.top
					left:		parent.left
					right:		vertScroll.left
					bottom:		parent.bottom
					margins:	jaspTheme.generalAnchorMargin
				}


				Item
				{
					//This item is here to make sure the text always stays down and without jumping around https://www.youtube.com/watch?v=XhzpxjuwZy0

					id:			bottomFeeder
					height:		Math.max(outputWindow.implicitHeight, outputScroll.height)
					width:		outputScroll.width

					TextArea
					{
						id:					outputWindow
						text:				rCmd.output
						font:				jaspTheme.fontRCode
						wrapMode:			TextEdit.Wrap
						color:				jaspTheme.textEnabled
						selectedTextColor:	jaspTheme.textDisabled
						selectionColor:		jaspTheme.black
						selectByMouse:		true
						readOnly:			true

						anchors
						{
							left:		parent.left
							right:		parent.right
							bottom:		parent.bottom
						}
						//textFormat:			TextEdit.RichText //too much trouble
					}
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
			height: 100 * preferencesModel.uiScale;// Math.min(Math.max(codeEntry.implicitHeight, 20 * jaspTheme.uiScale), 200 * jaspTheme.uiScale)
			anchors
			{
			    left:	parent.left
				right:	parent.right
				bottom:	parent.bottom
			}

			MouseArea
			{
				z:					-1
				acceptedButtons:	Qt.NoButton
				anchors.fill:		parent
				onWheel:			codeEntryScrollbar.scrollWheel(wheel)
			}

			Rectangle
			{
				id:				codeRect
				border.color:	jaspTheme.uiBorder
				color:			jaspTheme.white
				clip:			true

				//ScrollView
				Flickable
				{
					id:					codeEntryFlickable

					Item
					{
						width:					codeEntry.width
						height:					codeEntry.implicitHeight

						TextArea
						{
							id:						codeEntry
							font:					jaspTheme.fontRCode
							color:					jaspTheme.textEnabled
							selectedTextColor:		jaspTheme.textDisabled
							selectionColor:			jaspTheme.black
							selectByMouse:			true
							wrapMode:				TextEdit.Wrap
							focus:					true
							width:					codeRect.width


							placeholderText:		mainWindow.dataAvailable ? qsTr("Enter your R code here.\nThe data is fully available as 'data' and filtered as 'filteredData'.") : qsTr("Enter your R code here.")
							placeholderTextColor:	jaspTheme.grayDarker

							Shortcut { onActivated: runButton.runCode();	sequences: ["Ctrl+Enter", "Ctrl+Return", Qt.Key_F5];}
							Shortcut { onActivated: codeEntry.undo();		sequences: ["Ctrl+Z", ];}
							Shortcut { onActivated: codeEntry.selectAll();	sequences: ["Ctrl+A", ];}
						}
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
					right:		runButton.left
					left:		parent.left
					bottom:		parent.bottom
					margins:	jaspTheme.generalAnchorMargin
				}
			}

			JW.RoundedButton
			{
				id:				runButton
				text:			qsTr("Run Code")
				onClicked:		runCode();
				width:			clearOutput.width
				enabled:		codeEntry.text != "" && !rCmd.running

				toolTip:		qsTr("Pressing Ctrl+Enter or F5 will also run the code")

				function runCode() { if(runButton.enabled && rCmd.runCode(codeEntry.text)) codeEntry.text = ""; }

				anchors
				{
					top:			parent.top
					right:			parent.right
					bottom:			parent.verticalCenter
					margins:		jaspTheme.generalAnchorMargin
					bottomMargin:	jaspTheme.generalAnchorMargin * 0.5
				}

				Rectangle
				{
					visible:		rCmd.running
					color:			runButton.color
					border.color:	runButton.border.color
					border.width:	runButton.border.width
					anchors.fill:	parent

					LoadingIndicator
					{
						id:				runningIcon
						visible:		rCmd.running
						z:				2049
						anchors.fill:	parent
						//opacity:		0.5
					}
				}
			}

			JW.RoundedButton
			{
				id:		clearOutput
				text:		qsTr("Clear Output")
				onClicked:	rCmd.output = qsTr("Cleared...");
				width:		Math.max(clearOutput.implicitWidth, runButton.implicitWidth)

				anchors
				{
					top:		parent.verticalCenter
					right:		parent.right
					bottom:		parent.bottom
					margins:	jaspTheme.generalAnchorMargin
					topMargin:	jaspTheme.generalAnchorMargin * 0.5
				}
			}
		}
	}
}
