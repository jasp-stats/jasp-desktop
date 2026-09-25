//
// Copyright (C) 2013-2026 University of Amsterdam
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

// Runs Python scripts that call JASP through the jasp module (see PythonScriptRunner).
// MainWindow loads it once and shows it again after it was closed, so the script and its output stay.
Window
{
	id:						pythonRoot
	objectName:				"pythonScriptWindow"
	title:					scriptPath === "" ? qsTr("Python in JASP") : qsTr("Python in JASP - %1").arg(fileName(scriptPath))
	visible:				true
	width:					800 * preferencesModel.uiScale
	height:					600 * preferencesModel.uiScale
	flags:					Qt.Window | Qt.WindowFullscreenButtonHint
	color:					jaspTheme.white

	minimumWidth:			400 * preferencesModel.uiScale
	minimumHeight:			300 * preferencesModel.uiScale

	/// The .py file the script was last opened from or saved to
	property string			scriptPath:		""

	function fileName(path)	{ return path.substring(Math.max(path.lastIndexOf("/"), path.lastIndexOf("\\")) + 1); }

	function runScript()
	{
		if (runButton.enabled)
			runner.run(codeEntry.text);
	}

	function openScript()
	{
		var path = messages.browseOpenFileDocumentsQML(qsTr("Open a Python script"), qsTr("Python scripts (*.py)"), false);
		if (path === "")
			return;

		var code = runner.readScript(path); // undefined when it could not be read, the reason is then in the output
		if (code === undefined)
			return;

		codeEntry.text	= code;
		scriptPath		= path;
	}

	function saveScript(saveAs)
	{
		var path = scriptPath;

		if (saveAs || path === "")
			path = messages.browseSaveFileDocumentsQML(qsTr("Save the Python script"), qsTr("Python scripts (*.py)"));

		if (path !== "" && runner.writeScript(path, codeEntry.text))
			scriptPath = path;
	}

	function scrollOutputDown()
	{
		outputScroll.contentY = Math.max(0, outputScroll.contentHeight - outputScroll.height);
	}

	Shortcut { onActivated: pythonRoot.close();				sequences: ["Ctrl+W"];								}
	Shortcut { onActivated: pythonRoot.runScript();			sequences: ["Ctrl+Enter", "Ctrl+Return", "F5"];		}
	Shortcut { onActivated: pythonRoot.openScript();		sequences: ["Ctrl+O"];								}
	Shortcut { onActivated: pythonRoot.saveScript(false);	sequences: ["Ctrl+S"];								}

	PythonScriptRunner
	{
		id:				runner
		interpreter:	preferencesModel.pythonInterpreter
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

		// The script
		Item
		{
			id:								scriptPanel
			SplitView.minimumHeight:		100 * preferencesModel.uiScale
			SplitView.preferredHeight:		parent.height * 0.6

			Rectangle
			{
				anchors.fill:		parent
				anchors.margins:	jaspTheme.generalAnchorMargin
				border.color:		jaspTheme.uiBorder
				color:				jaspTheme.white
				clip:				true

				JC.TextArea
				{
					id:					codeEntry
					width:				parent.width
					height:				parent.height
					radius:				0
					font:				jaspTheme.fontRCode
					wrapMode:			TextEdit.Wrap
					showLineNumber:		true
					focus:				true

					Text
					{
						anchors.top:		parent.top
						anchors.left:		parent.left
						anchors.margins:	8 * preferencesModel.uiScale
						anchors.leftMargin:	3 * jaspTheme.itemPadding
						text:				qsTr("Write a Python script here. With 'import jasp' it can call JASP, for instance:\n\nimport jasp\nprint(jasp.data_info())\n\nhelp(jasp) lists what it can do. Ctrl+Enter or F5 runs the script.")
						font:				jaspTheme.font
						color:				jaspTheme.grayDarker
						wrapMode:			Text.Wrap
						width:				parent.width - 16 * preferencesModel.uiScale
						visible:			codeEntry.text === ""
					}
				}
			}
		}

		// What the script prints
		Item
		{
			id:							outputPanel
			SplitView.minimumHeight:	100 * preferencesModel.uiScale

			Column
			{
				anchors.fill:			parent
				anchors.margins:		jaspTheme.generalAnchorMargin
				spacing:				jaspTheme.generalAnchorMargin

				Rectangle
				{
					id:					outputContainer
					width:				parent.width
					height:				parent.height - buttonRow.height - parent.spacing
					border.color:		jaspTheme.uiBorder
					color:				jaspTheme.white
					clip:				true

					Flickable
					{
						id:						outputScroll
						contentHeight:			outputFeeder.height
						contentWidth:			width
						flickableDirection:		Flickable.VerticalFlick
						interactive:			false
						onContentHeightChanged:	Qt.callLater(pythonRoot.scrollOutputDown)

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
							id:					outputFeeder
							height:				Math.max(outputWindow.implicitHeight, outputScroll.height)
							width:				outputScroll.width

							TextArea
							{
								id:					outputWindow
								text:				runner.output
								font:				jaspTheme.fontRCode
								wrapMode:			TextEdit.Wrap
								color:				jaspTheme.textEnabled
								selectedTextColor:	jaspTheme.textDisabled
								selectionColor:		jaspTheme.black
								selectByMouse:		true
								readOnly:			true

								anchors
								{
									left:			parent.left
									right:			parent.right
									bottom:			parent.bottom
								}
							}

							Text
							{
								anchors.top:		parent.top
								anchors.left:		parent.left
								anchors.margins:	jaspTheme.generalAnchorMargin
								text:				qsTr("What the script prints shows here.")
								font:				jaspTheme.font
								color:				jaspTheme.grayDarker
								visible:			runner.output === ""
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

				Row
				{
					id:					buttonRow
					width:				parent.width
					height:				30 * preferencesModel.uiScale
					spacing:			jaspTheme.generalAnchorMargin

					readonly property real buttonWidth: (width - 5 * spacing) / 6

					JC.RectangularButton
					{
						id:				runButton
						text:			qsTr("Run script")
						onClicked:		pythonRoot.runScript()
						width:			buttonRow.buttonWidth
						height:			parent.height
						enabled:		codeEntry.text !== "" && !runner.running
						toolTip:		qsTr("Pressing Ctrl+Enter or F5 will also run the script")

						Rectangle
						{
							visible:		runner.running
							color:			runButton.color
							border.color:	runButton.border.color
							border.width:	runButton.border.width
							anchors.fill:	parent

							LoadingIndicator
							{
								visible:		runner.running
								z:				2049
								anchors.fill:	parent
							}
						}
					}

					JC.RectangularButton
					{
						text:			qsTr("Stop")
						onClicked:		runner.stop()
						width:			buttonRow.buttonWidth
						height:			parent.height
						enabled:		runner.running
						toolTip:		qsTr("End the running script at once")
					}

					JC.RectangularButton
					{
						text:			qsTr("Clear output")
						onClicked:		runner.clearOutput()
						width:			buttonRow.buttonWidth
						height:			parent.height
					}

					JC.RectangularButton
					{
						text:			qsTr("Open...")
						onClicked:		pythonRoot.openScript()
						width:			buttonRow.buttonWidth
						height:			parent.height
					}

					JC.RectangularButton
					{
						text:			qsTr("Save")
						onClicked:		pythonRoot.saveScript(false)
						width:			buttonRow.buttonWidth
						height:			parent.height
						enabled:		codeEntry.text !== ""
					}

					JC.RectangularButton
					{
						text:			qsTr("Save as...")
						onClicked:		pythonRoot.saveScript(true)
						width:			buttonRow.buttonWidth
						height:			parent.height
						enabled:		codeEntry.text !== ""
					}
				}
			}
		}
	}
}
