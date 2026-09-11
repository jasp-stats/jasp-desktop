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
import JASP.Widgets
import JASP.Controls

PrefsScrollView
{
	id:		batchView

	property real	labelWidth:		Math.max(inputFileButton.implicitWidth, inputFolderButton.implicitWidth, outputFolderButton.implicitWidth)
	property var	batch:			fileMenuModel.batch

	MenuHeader
	{
		id:					menuHeader
		headertext:			qsTr("Batch")
		helpMD:				allHelp.Batch
		addMargin:			false
		activeFocusOnTab:	true
	}

	ErrorMessage
	{
		width:		parent.width
		warning:	true
		text:		batchView.batch.jaspFile === ""	? qsTr("Batch runs the analyses of a JASP file against other data files, so open a JASP file (or save your work as one) before running a batch.")
				  : batchView.batch.jaspFileModified ? qsTr("The workspace has changes that were not saved yet. A batch starts from the JASP file <i>as it is on disk</i>, so save it first if you want your latest changes to be used.")
				  :									   ""
	}

	PrefsGroupRect
	{
		title:	qsTr("JASP file to run")

		PrefsTextInput
		{
			id:					jaspFileText
			width:				parent.width
			text:				batchView.batch.jaspFile === "" ? qsTr("<no JASP file opened>") : batchView.batch.jaspFile
			textInput.readOnly:	true
			toolTip:			qsTr("Every data file is run against this JASP file, which is the one you have open.")
		}
	}

	PrefsGroupRect
	{
		id:		inputGroup
		title:	qsTr("Data files to run it against")

		RadioButtonGroup
		{
			id:	inputKind

			RadioButton
			{
				id:					oneFileButton
				label:				qsTr("One data file")
				checked:			!batchView.batch.useInputFolder
				onCheckedChanged:	if(checked) batchView.batch.useInputFolder = false
				toolTip:			qsTr("Run the JASP file against a single data file.")
				KeyNavigation.tab:	aFolderButton
			}

			RadioButton
			{
				id:					aFolderButton
				label:				qsTr("A folder of data files")
				checked:			batchView.batch.useInputFolder
				onCheckedChanged:	if(checked) batchView.batch.useInputFolder = true
				toolTip:			qsTr("Run the JASP file against every data file in a folder and its subfolders. Each data file gets its own JASP, so one data file cannot influence the next.")
				KeyNavigation.tab:	inputFileButton
			}
		}

		Item
		{
			width:		parent.width
			height:		inputFileButton.height
			enabled:	!batchView.batch.useInputFolder

			RoundedButton
			{
				id:						inputFileButton
				text:					qsTr("Data file:")
				width:					batchView.labelWidth
				onClicked:				batchView.batch.browseInputFile()
				toolTip:				qsTr("Browse to the data file to run the analyses on.")
				activeFocusOnTab:		true
				KeyNavigation.tab:		inputFileText.textInput

				anchors
				{
					left:				parent.left
					leftMargin:			jaspTheme.subOptionOffset
					verticalCenter:		parent.verticalCenter
				}
			}

			PrefsTextInput
			{
				id:					inputFileText
				text:				batchView.batch.inputFile
				onEditingFinished:	batchView.batch.inputFile = text
				nextEl:				inputFolderButton
				height:				inputFileButton.height

				anchors
				{
					left:			inputFileButton.right
					right:			parent.right
					margins:		jaspTheme.generalAnchorMargin
					verticalCenter:	parent.verticalCenter
				}
			}
		}

		Item
		{
			width:		parent.width
			height:		inputFolderButton.height
			enabled:	batchView.batch.useInputFolder

			RoundedButton
			{
				id:						inputFolderButton
				text:					qsTr("Data folder:")
				width:					batchView.labelWidth
				onClicked:				batchView.batch.browseInputFolder()
				toolTip:				qsTr("Browse to the folder holding the data files. Its subfolders are used as well, files JASP cannot import (and .jasp, .html and .pdf files) are skipped.")
				activeFocusOnTab:		true
				KeyNavigation.tab:		inputFolderText.textInput

				anchors
				{
					left:				parent.left
					leftMargin:			jaspTheme.subOptionOffset
					verticalCenter:		parent.verticalCenter
				}
			}

			PrefsTextInput
			{
				id:					inputFolderText
				text:				batchView.batch.inputFolder
				onEditingFinished:	batchView.batch.inputFolder = text
				nextEl:				outputFolderButton
				height:				inputFolderButton.height

				anchors
				{
					left:			inputFolderButton.right
					right:			parent.right
					margins:		jaspTheme.generalAnchorMargin
					verticalCenter:	parent.verticalCenter
				}
			}
		}
	}

	PrefsGroupRect
	{
		id:		outputGroup
		title:	qsTr("Results")

		Item
		{
			width:	parent.width
			height:	outputFolderButton.height

			RoundedButton
			{
				id:						outputFolderButton
				text:					qsTr("Output folder:")
				width:					batchView.labelWidth
				onClicked:				batchView.batch.browseOutputFolder()
				toolTip:				qsTr("Browse to the folder the results should be written to. Leave it empty to write each result next to its own data file.")
				activeFocusOnTab:		true
				KeyNavigation.tab:		outputFolderText.textInput

				anchors
				{
					left:				parent.left
					leftMargin:			jaspTheme.subOptionOffset
					verticalCenter:		parent.verticalCenter
				}
			}

			PrefsTextInput
			{
				id:					outputFolderText
				text:				batchView.batch.outputFolder
				onEditingFinished:	batchView.batch.outputFolder = text
				nextEl:				exportTypeDropDown
				toolTip:			qsTr("Leave empty to write each result next to its own data file.")
				height:				outputFolderButton.height

				anchors
				{
					left:			outputFolderButton.right
					right:			parent.right
					margins:		jaspTheme.generalAnchorMargin
					verticalCenter:	parent.verticalCenter
				}
			}
		}

		DropDown
		{
			id:						exportTypeDropDown
			label:					qsTr("Export the results as:")
			//Keep these in the same order as BatchFileMenu::exportTypes
			values:					[qsTr("HTML"), qsTr("PDF"), qsTr("JASP file"), qsTr("Do not export")]
			toolTip:				qsTr("Which kind of result is written per data file. \"Do not export\" only synchronizes and refreshes, which is a good way to check that a set of data files runs through without errors.")
			startValue:				values[batchView.batch.exportTypeIndex]
			onActivated:			(index) => { batchView.batch.exportTypeIndex = index }

			anchors.left:			parent.left
			anchors.leftMargin:		jaspTheme.subOptionOffset
		}
	}

	PrefsGroupRect
	{
		id:		optionsGroup
		title:	qsTr("Options")

		CheckBox
		{
			id:					keepJASPOpen
			label:				qsTr("Open a JASP window for each data file")
			checked:			batchView.batch.keepJASPOpen
			onCheckedChanged:	batchView.batch.keepJASPOpen = checked
			toolTip:			qsTr("Show the JASP that is started and leave it open at the end instead of closing it, so you can look at the result. Every data file gets its own, all open at the same time, so a folder of twenty data files leaves you with twenty JASPs to close again. These JASPs also take the focus, where a batch that closes itself again leaves you working undisturbed.")
			KeyNavigation.tab:	keepMissingCols
		}

		CheckBox
		{
			id:					keepMissingCols
			label:				qsTr("Keep columns missing from the data file")
			checked:			batchView.batch.keepMissingColsWhenSyncing
			onCheckedChanged:	batchView.batch.keepMissingColsWhenSyncing = checked
			toolTip:			qsTr("Keep the columns the JASP file uses but the data file does not have, with the data they already had, instead of removing them. The columns of the data file are then added next to the ones already there.")
			KeyNavigation.tab:	runButton
		}
	}

	PrefsGroupRect
	{
		id:		runGroup
		title:	qsTr("Run")

		Text
		{
			width:		parent.width
			text:		qsTr("This is what JASP runs, you can also use it from a terminal or a script:")
			font:		jaspTheme.font
			color:		jaspTheme.textEnabled
			wrapMode:	Text.Wrap
		}

		Rectangle
		{
			width:			parent.width
			height:			commandLineText.height + (2 * jaspTheme.generalAnchorMargin)
			color:			jaspTheme.white
			border.color:	jaspTheme.borderColor
			border.width:	1
			radius:			jaspTheme.borderRadius

			TextEdit
			{
				id:					commandLineText
				text:				batchView.batch.commandLine
				font:				jaspTheme.fontCode
				color:				jaspTheme.textEnabled
				wrapMode:			TextEdit.Wrap
				readOnly:			true
				selectByMouse:		true

				anchors
				{
					left:			parent.left
					right:			parent.right
					top:			parent.top
					margins:		jaspTheme.generalAnchorMargin
				}
			}
		}

		Item
		{
			width:	parent.width
			height:	runButton.height

			RoundedButton
			{
				id:					runButton
				text:				batchView.batch.running ? qsTr("Running...") : qsTr("Run batch")
				enabled:			batchView.batch.readyToRun
				onClicked:			batchView.batch.runBatch()
				toolTip:			batchView.batch.problem !== "" ? batchView.batch.problem : qsTr("Start JASP with the commandline above. Keep this JASP open while the batch runs.")
				activeFocusOnTab:	true
				KeyNavigation.tab:	stopButton

				anchors.left:		parent.left
			}

			RoundedButton
			{
				id:					stopButton
				text:				qsTr("Stop")
				enabled:			batchView.batch.running
				onClicked:			batchView.batch.stopBatch()
				toolTip:			qsTr("Stop the running batch. The data files it already processed keep their results.")
				activeFocusOnTab:	true
				KeyNavigation.tab:	clearButton

				anchors.left:		runButton.right
				anchors.leftMargin:	jaspTheme.generalAnchorMargin
			}

			RoundedButton
			{
				id:					clearButton
				text:				qsTr("Clear output")
				enabled:			batchView.batch.output !== ""
				onClicked:			batchView.batch.clearOutput()
				activeFocusOnTab:	true

				anchors.right:		parent.right
			}
		}

		Text
		{
			width:		parent.width
			visible:	batchView.batch.problem !== ""
			text:		batchView.batch.problem
			font:		jaspTheme.font
			color:		jaspTheme.controlErrorTextColor
			wrapMode:	Text.Wrap
		}

		Rectangle
		{
			width:			parent.width
			height:			250 * preferencesModel.uiScale
			visible:		batchView.batch.output !== ""
			color:			jaspTheme.white
			border.color:	jaspTheme.borderColor
			border.width:	1
			radius:			jaspTheme.borderRadius

			Flickable
			{
				id:					outputFlickable
				anchors.fill:		parent
				anchors.margins:	jaspTheme.generalAnchorMargin
				clip:				true
				boundsBehavior:		Flickable.StopAtBounds
				contentWidth:		width
				contentHeight:		outputText.height

				//Follow the batch while it runs, but leave the scrolling alone as long as nothing new comes in
				onContentHeightChanged:	contentY = Math.max(0, contentHeight - height)

				TextEdit
				{
					id:				outputText
					width:			outputFlickable.width
					text:			batchView.batch.output
					font:			jaspTheme.fontCode
					color:			jaspTheme.textEnabled
					wrapMode:		TextEdit.Wrap
					readOnly:		true
					selectByMouse:	true
				}
			}
		}
	}
}
