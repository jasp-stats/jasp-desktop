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
import QtQuick.Controls as QTC
import JASP.Widgets
import JASP.Controls

PrefsScrollView
{
	id:		batchView

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

	//Not an option but an indication: the JASP file is simply the one you have open, so this is
	//plain text rather than a group with a field, which would suggest there is something to pick.
	Column
	{
		width:		parent.width
		spacing:	jaspTheme.generalAnchorMargin

		Text
		{
			width:		parent.width
			text:		qsTr("Every data file is run against the current JASP file:")
			font:		jaspTheme.font
			color:		jaspTheme.textEnabled
			wrapMode:	Text.Wrap
		}

		Text
		{
			width:			parent.width
			leftPadding:	jaspTheme.subOptionOffset
			text:			batchView.batch.jaspFile === "" ? qsTr("<no JASP file opened>") : batchView.batch.jaspFile
			textFormat:		Text.PlainText //A path can hold '<' or '&', and AutoText would swallow "<no JASP file opened>" as a tag
			font:			jaspTheme.font
			color:			jaspTheme.textEnabled
			wrapMode:		Text.Wrap
		}
	}

	PrefsGroupRect
	{
		id:		inputGroup
		title:	qsTr("Data files to run it against")

		Item
		{
			width:	parent.width
			height:	addDataFilesButton.height

			RoundedButton
			{
				id:					addDataFilesButton
				text:				qsTr("Add files...")
				onClicked:			batchView.batch.browseDataFiles()
				toolTip:			qsTr("Browse to the data files to run the JASP file against, you can select several at once.")
				activeFocusOnTab:	true
				KeyNavigation.tab:	addDataFolderButton
				anchors.left:		parent.left
			}

			RoundedButton
			{
				id:					addDataFolderButton
				text:				qsTr("Add folder...")
				onClicked:			batchView.batch.browseDataFolder()
				toolTip:			qsTr("Browse to a folder holding data files. All data files in it and in its subfolders are listed, so you can deselect the ones you do not want. Files JASP cannot import (such as .jasp, .html and .pdf files) are skipped.")
				activeFocusOnTab:	true
				KeyNavigation.tab:	outputFolderButton
				anchors.left:		addDataFilesButton.right
				anchors.leftMargin:	jaspTheme.generalAnchorMargin
			}
		}

		Rectangle
		{
			width:			parent.width
			height:			inputsList.height + (2 * jaspTheme.generalAnchorMargin)
			color:			jaspTheme.white
			border.color:	jaspTheme.borderColor
			border.width:	1
			radius:			jaspTheme.borderRadius

			FontMetrics
			{
				id:		inputsFontMetrics
				font:	jaspTheme.font
			}

			Text
			{
				anchors.fill:		inputsList
				visible:			inputsList.count === 0
				text:				qsTr("No data files yet.")
				font:				jaspTheme.font
				color:				jaspTheme.textDisabled
				verticalAlignment:	Text.AlignVCenter
				elide:				Text.ElideRight
			}

			ListView
			{
				id:					inputsList
				model:				batchView.batch.inputs
				height:				Math.max(1, Math.min(count, maxVisibleRows)) * rowHeight
				interactive:		count > maxVisibleRows //Until then the scrolling is left to the page
				clip:				true
				boundsBehavior:		Flickable.StopAtBounds

				readonly property int	maxVisibleRows:	10
				readonly property real	rowHeight:		Math.ceil(inputsFontMetrics.height) + jaspTheme.generalAnchorMargin

				anchors
				{
					top:		parent.top
					left:		parent.left
					right:		parent.right
					margins:	jaspTheme.generalAnchorMargin
				}

				delegate: Rectangle
				{
					id:		inputRow
					width:	inputsList.width
					height:	inputsList.rowHeight
					color:	inputHover.containsMouse ? jaspTheme.itemHoverColor : "transparent"

					required property int		index
					required property bool		isFolder
					required property string	label
					required property string	path
					required property bool		selected
					required property bool		inFolder
					required property bool		removable
					required property int		selectedCount
					required property int		dataFileCount

					MouseArea
					{
						id:				inputHover
						anchors.fill:	parent
						hoverEnabled:	true
						cursorShape:	Qt.PointingHandCursor
						onClicked:		batchView.batch.inputs.setSelected(inputRow.index, !inputRow.selected) //The whole row (de)selects, not only its box

						QTC.ToolTip.text:		inputRow.path
						QTC.ToolTip.visible:	inputLabel.truncated && containsMouse && !removeInput.containsMouse

						//The box of a JASP CheckBox, which itself is a form control: too much for every row of a list,
						//and clicking it would break the binding to the model that a click on the row of its folder has to update.
						Rectangle
						{
							id:				selectedBox
							height:			inputsFontMetrics.height
							width:			height
							color:			inputRow.selected ? jaspTheme.buttonBackgroundColor : jaspTheme.controlBackgroundColor
							border.color:	inputRow.selected ? jaspTheme.buttonBackgroundColor : jaspTheme.borderColor
							border.width:	1
							radius:			jaspTheme.borderRadius

							anchors
							{
								left:			parent.left
								leftMargin:		(inputRow.inFolder ? jaspTheme.subOptionOffset : 0) + (jaspTheme.generalAnchorMargin / 2)
								verticalCenter:	parent.verticalCenter
							}

							Text
							{
								visible:			inputRow.selected
								color:				jaspTheme.white
								text:				"✓"
								font:				jaspTheme.font
								anchors.centerIn:	parent
								renderType:			Text.QtRendering
							}
						}

						Image
						{
							id:					folderIcon
							visible:			inputRow.isFolder
							height:				inputsFontMetrics.height
							width:				visible ? height : 0
							sourceSize.width:	height * 2
							sourceSize.height:	height * 2
							fillMode:			Image.PreserveAspectFit
							source:				jaspTheme.iconPath + "/folder.svg"

							anchors
							{
								left:			selectedBox.right
								leftMargin:		visible ? jaspTheme.generalAnchorMargin : 0
								verticalCenter:	parent.verticalCenter
							}
						}

						Text
						{
							id:				inputLabel
							text:			inputRow.label
							textFormat:		Text.PlainText	//A path can hold '<' or '&'
							font:			jaspTheme.font
							color:			jaspTheme.textEnabled
							elide:			Text.ElideMiddle	//The start and the end of a path tell the most about it

							anchors
							{
								left:			folderIcon.right
								leftMargin:		jaspTheme.generalAnchorMargin
								right:			folderCount.left
								rightMargin:	jaspTheme.generalAnchorMargin
								verticalCenter:	parent.verticalCenter
							}
						}

						Text
						{
							id:				folderCount
							visible:		inputRow.isFolder
							width:			visible ? implicitWidth : 0
							text:			inputRow.dataFileCount === 0 ? qsTr("no data files") : qsTr("%1 of %2 selected").arg(inputRow.selectedCount).arg(inputRow.dataFileCount)
							font:			jaspTheme.font
							color:			jaspTheme.textDisabled

							anchors
							{
								right:			removeInput.left
								verticalCenter:	parent.verticalCenter
							}
						}

						MouseArea
						{
							id:				removeInput
							visible:		inputRow.removable
							height:			parent.height
							width:			height
							anchors.right:	parent.right
							hoverEnabled:	true
							cursorShape:	Qt.PointingHandCursor
							onClicked:		batchView.batch.inputs.remove(inputRow.index)

							QTC.ToolTip.text:		inputRow.isFolder ? qsTr("Remove this folder and its data files from the list") : qsTr("Remove from the list")
							QTC.ToolTip.visible:	containsMouse

							Image
							{
								anchors.fill:	parent
								source:			jaspTheme.iconPath + "/subtraction-sign-small.svg"
								visible:		inputHover.containsMouse
							}
						}
					}
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
				onClicked:				batchView.batch.browseOutputFolder()
				toolTip:				qsTr("Browse to the folder the results should be written to. Leave it empty to write each result next to its own data file.")
				activeFocusOnTab:		true
				KeyNavigation.tab:		outputFolderText.textInput

				anchors
				{
					left:				parent.left
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
			KeyNavigation.tab:	copyCommandLineButton
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

		Item
		{
			width:	parent.width
			height:	Math.max(commandLineBox.height, copyCommandLineButton.height)

			Rectangle
			{
				id:				commandLineBox
				height:			commandLineText.height + (2 * jaspTheme.generalAnchorMargin)
				color:			jaspTheme.controlDisabledBackgroundColor
				border.color:	jaspTheme.borderColor
				border.width:	1
				radius:			jaspTheme.borderRadius

				anchors
				{
					top:			parent.top
					left:			parent.left
					right:			copyCommandLineButton.left
					rightMargin:	jaspTheme.generalAnchorMargin
				}

				//Only there to be read (and copied with the button next to it), so it looks like a disabled field
				TextEdit
				{
					id:			commandLineText
					text:		batchView.batch.commandLine
					font:		jaspTheme.fontCode
					color:		jaspTheme.textDisabled
					wrapMode:	TextEdit.Wrap
					readOnly:	true
					enabled:	false

					anchors
					{
						left:		parent.left
						right:		parent.right
						top:		parent.top
						margins:	jaspTheme.generalAnchorMargin
					}
				}
			}

			RoundedButton
			{
				id:					copyCommandLineButton
				text:				qsTr("Copy")
				iconSource:			jaspTheme.iconPath + (justCopied ? "/check-mark.png" : "/menu-data-copy.svg")
				showIconAndText:	true
				iconLeft:			false	//RectangularButton only leaves room for the icon next to the text
				centerText:			false	//when the text is on the left and the icon on the right
				toolTip:			justCopied ? qsTr("Copied to the clipboard") : qsTr("Copy the commandline to the clipboard")
				activeFocusOnTab:	true
				KeyNavigation.tab:	runButton
				anchors.top:		parent.top
				anchors.right:		parent.right

				property bool		justCopied:	false //Shows the copy worked, for a moment, without changing the size of the button

				onClicked:
				{
					batchView.batch.copyCommandLine()
					justCopied = true
					justCopiedTimer.restart()
				}

				Timer
				{
					id:				justCopiedTimer
					interval:		2000
					onTriggered:	copyCommandLineButton.justCopied = false
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
				showIconAndText:	batchView.batch.running	//Makes room for the LoadingIndicator, in the place of the icon
				iconLeft:			false
				centerText:			!batchView.batch.running

				anchors.left:		parent.left

				LoadingIndicator
				{
					id:						runningIndicator
					x:						runButton.icon.x
					y:						runButton.icon.y
					width:					runButton.icon.width
					height:					runButton.icon.height
					visible:				batchView.batch.running
					autoStartOnVisibility:	false	//That only starts turning when it becomes visible, not when it already is as the page opens during a batch

					function followRunning() { if(batchView.batch.running) startManually(); else stopManually(); }

					Component.onCompleted:	followRunning()

					Connections
					{
						target:						batchView.batch
						function onRunningChanged()	{ runningIndicator.followRunning() }
					}
				}
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
			color:			jaspTheme.controlDisabledBackgroundColor
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

				//Disabled like the commandline, the Flickable around it still scrolls it
				TextEdit
				{
					id:				outputText
					width:			outputFlickable.width
					text:			batchView.batch.output
					font:			jaspTheme.fontCode
					color:			jaspTheme.textDisabled
					wrapMode:		TextEdit.Wrap
					readOnly:		true
					enabled:		false
				}
			}
		}
	}
}
