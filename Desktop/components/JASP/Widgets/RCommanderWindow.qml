//
// Copyright (C) 2013-2025 University of Amsterdam
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

import QtQuick
import QtQuick.Window
import QtQuick.Controls
import JASP
import JASP.Controls		as JC

Window
{
	id:						rcmdRoot

	Accessible.name:		title

	title:					qsTr("R in JASP")
	visible:				true
	width:					800 * preferencesModel.uiScale
	height:					600 * preferencesModel.uiScale
	flags:					Qt.Window | Qt.WindowFullscreenButtonHint
	color:					jaspTheme.white

	minimumWidth:			400 * preferencesModel.uiScale
	minimumHeight:			300 * preferencesModel.uiScale

	Shortcut {onActivated:	rcmdRoot.close();				sequences: ["Ctrl+W"];				}

	RCommander
	{
		id:					rCmd
		output:				qsTr("Welcome to R in JASP!");
		onScrollDown:		outputScroll.contentY = outputScroll.contentHeight - outputScroll.height;
		onCloseWindow:		rcmdRoot.close()
		onActivated:
		{
			rcmdRoot.visible = false;
			rcmdRoot.visible = true;
		}
	}

	SplitView
	{
		id:					splitView
		anchors.fill:		parent
		orientation:		Qt.Vertical

		handle: Rectangle
		{
			implicitHeight:			8
			color:					SplitHandle.pressed || SplitHandle.hovered ? jaspTheme.uiBorder : jaspTheme.white
		}

		// Output Panel
		Item
		{
			id:						outputPanel
			SplitView.minimumHeight: 100 * preferencesModel.uiScale
			SplitView.preferredHeight: parent.height * 0.6

			Rectangle
			{
				id:					outputContainer
				anchors.fill:		parent
				anchors.margins:	jaspTheme.generalAnchorMargin
				border.color:		jaspTheme.uiBorder
				color:				jaspTheme.white
				clip:				true

				Flickable
				{
					id:						outputScroll
					contentHeight:			bottomFeeder.height
					contentWidth:			width
					flickableDirection:		Flickable.VerticalFlick
					interactive:			false
					onContentHeightChanged: if(contentHeight > height) rCmd.countDownToScroll();

					anchors
					{
						top:				parent.top
						left:				parent.left
						right:				vertScroll.left
						bottom:				parent.bottom
						margins:			jaspTheme.generalAnchorMargin
					}

					Item
					{
						id:					bottomFeeder
						height:				Math.max(outputWindow.implicitHeight, outputScroll.height)
						width:				outputScroll.width

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

							JC.RSyntaxHighlighterQuick
							{
								textDocument:	outputWindow.textDocument
							}

							anchors
							{
								left:			parent.left
								right:			parent.right
								bottom:			parent.bottom
							}
						}
					}

					MouseArea
					{
						z:					-1
						acceptedButtons:	Qt.NoButton
						anchors.fill:		parent
						onWheel:			(wheel)=> { vertScroll.scrollWheel(wheel); }
					}
				}

				JC.JASPScrollBar
				{
					id:						vertScroll
					flickable:				outputScroll
					manualAnchor:			true

					anchors
					{
						top:				parent.top
						right:				parent.right
						bottom:				parent.bottom
					}
				}
			}
		}

		// Input Panel
		Item
		{
			id:							inputPanel
			SplitView.minimumHeight:	100 * preferencesModel.uiScale

			Column
			{
				anchors.fill:			parent
				anchors.margins:		jaspTheme.generalAnchorMargin
				spacing:				jaspTheme.generalAnchorMargin

				Rectangle
				{
					id:					codeRect
					width:				parent.width
					height:				parent.height - buttonRow.height - parent.spacing // * 2
					border.color:		jaspTheme.uiBorder
					color:				jaspTheme.white
					clip:				true

					JC.TextArea
					{
						JC.RSyntaxHighlighterQuick
						{
							textDocument:	codeEntry.textDocument
						}

						id:						codeEntry
						width:					parent.width
						height:					parent.height
						radius:					0
						font:					jaspTheme.fontRCode
						wrapMode:				TextEdit.Wrap
						showLineNumber:			true
						focus:					true

						Text
						{
							id:					customPlaceholder
							anchors.top:		parent.top
							anchors.left:		parent.left
							anchors.margins:	8 * preferencesModel.uiScale
							anchors.leftMargin:	3 * jaspTheme.itemPadding
							text:				mainWindow.dataAvailable ?
													qsTr("Enter your R code here.\nThe data is available unfiltered as '%1'\nand filtered as '%2'.\nYou can also paste syntax-mode JASP analyses here.").arg("data").arg("filteredData") :
													qsTr("Enter your R code here.\n\nYou can also paste syntax-mode JASP analyses here.")
							font:				jaspTheme.font
							color:				jaspTheme.grayDarker
							wrapMode:			Text.Wrap
							width:				parent.width - 16 * preferencesModel.uiScale
							visible:			codeEntry.text === ""
						}

						Shortcut { onActivated:	runButton.runCode();	sequences: ["Ctrl+Enter", "Ctrl+Return", Qt.Key_F5];}
						Shortcut { onActivated:	codeEntry.undo();		sequences: ["Ctrl+Z"];}
						Shortcut { onActivated:	codeEntry.selectAll();	sequences: ["Ctrl+A"];}

						onTextChanged:
						{	rCmd.checkRCode(text)
							customPlaceholder.visible = text === ""
						}
					}
				}

				// Bottom button Row
				Row
				{
					id:					buttonRow
					width:				parent.width
					height:				30 * preferencesModel.uiScale
					spacing:			jaspTheme.generalAnchorMargin

					JC.RectangularButton
					{
						id:				runButton
						text:			qsTr("Run code")
						onClicked:		runCode()
						width:			(parent.width - 3 * parent.spacing) * 0.25
						height:			parent.height
						enabled:		codeEntry.text != "" && !rCmd.running && !rCmd.isAnalysisCode
						toolTip:		qsTr("Pressing Ctrl+Enter or F5 will also run the code")

						function runCode() {
							if(runButton.enabled && rCmd.runCode(codeEntry.text))
								codeEntry.text = "";
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
								id:			runningIcon
								visible:	rCmd.running
								z:			2049
								anchors.fill: parent
							}
						}
					}

					JC.RectangularButton
					{
						id:					addAnalysisItem
						text:				qsTr("Add analysis")
						onClicked:			addAnalysis()
						width:				runButton.width
						height:				parent.height
						enabled:			rCmd.isAnalysisCode && !rCmd.running

						function addAnalysis() {
							if(addAnalysisItem.enabled && rCmd.addAnalysis(codeEntry.text))
								codeEntry.text = "";
						}
					}

					JC.RectangularButton
					{
						id:					clearOutput
						text:				qsTr("Clear output")
						onClicked:			rCmd.output = qsTr("Cleared...");
						width:				runButton.width
						height:				parent.height
					}

					JC.DropDown
					{
						id:					selectModule
						values:				dynamicModules.loadedModulesTitles
						control.width:		Math.max(runButton.width, runButton.width)
						onValueChanged:		rCmd.loadModule(dynamicModules.loadedModules[currentIndex])
						control.height:		runButton.height
					}
				}
			}
		}
	}
}
