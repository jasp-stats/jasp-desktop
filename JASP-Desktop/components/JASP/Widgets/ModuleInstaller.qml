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

import QtQuick 2.11
import Qt.labs.folderlistmodel 2.11
import QtQuick.Window 2.3
import JASP.Theme 1.0
import "qrc:///qml"

Rectangle
{
	id:			moduleInstallerRect
	objectName: "moduleInstallerRect"
	color:		Theme.grayDarker

	width:		900
	height:		500

	signal closeWindow()

	property var currentJSON: null

	Item
	{
		id:					fileBrowseArea
		anchors.top:		parent.top
		anchors.left:		parent.left
		anchors.right:		parent.right
		anchors.bottom:		installButton.top

		onHeightChanged:
		{
			if(height <= Theme.generalAnchorMargin * 2)
				visible = false
		}

		Rectangle
		{
			id: folderRect
			anchors.top:		parent.top
			anchors.left:		parent.left
			anchors.right:		parent.horizontalCenter
			anchors.bottom:		parent.bottom
			anchors.margins:	Theme.generalAnchorMargin



			color:			Theme.whiteBroken
			border.color:	Theme.grayLighter
			border.width:	1

			ListView
			{
				id:					folderList
				anchors.fill:		parent
				anchors.margins:	Theme.generalAnchorMargin
				clip:				true
				spacing:			Theme.rowSpacing

				property string currentlySelectedFilePath: ""

				FolderListModel {
					id: folderModel

					nameFilters: ["*.jaspMod"]
					showDotAndDotDot: true
					showDirs: true
				}

				Component {
					id: fileDelegate
					FilterButton
					{
						id:					fileDelegateRect
						height:				32
						width:				folderList.width - (Theme.generalAnchorMargin * 2)
						x:					Theme.generalAnchorMargin
						showIconAndText:	true

						border.color:		Theme.grayDarker
						border.width:		1
						selected:			folderList.currentlySelectedFilePath !== "" && folderList.currentlySelectedFilePath === filePath
						disabled:			selected

						property int margins: 4


						iconSource: fileIsDir			? "qrc:///icons/folder.svg"			: "qrc:///icons/JASP_logo_green.svg"
						toolTip:	selected			? "Currently selected JASP Module"	: fileIsDir	? "a folder, click to enter" : "a JASP Module, click it to see more information"
						text:		fileName === ".."	? fileName + " (directory up)"		: fileName


						onClicked:
						{
							folderList.currentlySelectedFilePath = ""

							if(fileIsDir)
								folderModel.folder = "file:" + filePath
							else
							{
								if(dynamicModules.isFileAnArchive(filePath))
								{
									var textJson			= dynamicModules.getDescriptionFromArchive(filePath)
									descriptionViewer.text	= "<i>File is not a JASP module or something is wrong with it.</i>"

									moduleInstallerRect.currentJSON = JSON.parse(textJson)

									var moduleDescription	= moduleInstallerRect.currentJSON.moduleDescription

									var title				= moduleDescription.title
									var description			= moduleDescription.description
									var version				= moduleDescription.version
									var author				= moduleDescription.author
									var maintainer			= moduleDescription.maintainer
									var website				= moduleDescription.website

									if(title !== undefined)
									{
										descriptionViewer.text = "<h3>" + title + "</h3><i>Version " + version + "</i><br><p>" + description + "</p><br><br><i>Created by " + author + " and maintained by " + maintainer + ".<br>See website for further details: <a href=\"http://" + website + "\">" + website + "</a></i>"
										folderList.currentlySelectedFilePath = filePath
									}

								}
								else
									descriptionViewer.text = filePath + " is not a JASP Module!"
							}

						}

					}

				}

				model:		folderModel
				delegate:	fileDelegate
			}
		}

		Rectangle
		{
			anchors.top:		parent.top
			anchors.left:		folderRect.right
			anchors.right:		parent.right
			anchors.bottom:		parent.bottom
			anchors.margins:	Theme.generalAnchorMargin

			color:				Theme.whiteBroken
			border.color:		Theme.gray
			border.width:		1


			Text
			{
				id: descriptionViewer
				anchors.fill:		parent
				anchors.margins:	Theme.generalAnchorMargin
				text:				"<i>Click a JASP Module to see more information here</i>"
				textFormat:			Text.StyledText
				wrapMode:			Text.WrapAtWordBoundaryOrAnywhere
				onLinkActivated:	Qt.openUrlExternally(link)
				clip:				true

				MouseArea
				{
					anchors.fill:		parent
					acceptedButtons:	Qt.NoButton
					cursorShape:		descriptionViewer.hoveredLink !== "" ? Qt.PointingHandCursor : Qt.ArrowCursor
				}
			}
		}
	}

	FilterButton
	{
		id:					installButton
		anchors.left:		parent.left
		anchors.right:		parent.right
		anchors.bottom:		installProgressItem.top
		anchors.margins:	Theme.generalAnchorMargin
		disabled:			moduleInstallerRect.currentJSON === null

		text:				"Install"
		toolTip:			disabled ? "Select a JASP Module to install" : "Press this to install your selected Module"

		onClicked:
		{
			if(moduleInstallerRect.currentJSON !== null)
			{
				installProgressItem.height = Qt.binding(function() { return moduleInstallerRect.height - (installButton.height + (Theme.generalAnchorMargin * 2) )})

				installProgressItem.installText = "Installing Module '"+moduleInstallerRect.currentJSON.moduleDescription.title +"'"
				text							= installProgressItem.installText + "..."
				disabled						= true
				selected						= true


				dynamicModules.installJASPModule(folderList.currentlySelectedFilePath)
			}
		}
	}

	Item
	{
		id:				installProgressItem

		anchors.left:	parent.left
		anchors.right:	parent.right
		anchors.bottom: parent.bottom

		Behavior on height { PropertyAnimation {} }

		height: 0
		visible: height > 0

		property string installMsg:		dynamicModules.currentInstallMsg
		property bool	installDone:	dynamicModules.currentInstallDone
		property string installText:	""

		onInstallMsgChanged:	if(installMsg != "")	installText += "\n" + installMsg
		onInstallDoneChanged:	if(installDone)			installText += "\nInstall is finished!"

		Rectangle
		{
			anchors.top:		parent.top
			anchors.left:		parent.left
			anchors.right:		parent.right
			anchors.bottom:		closeButton.top
			anchors.margins:	Theme.generalAnchorMargin

			color:				Theme.whiteBroken
			border.color:		Theme.grayLighter
			border.width:		1

			Text
			{
				anchors.fill:		parent
				anchors.margins:	Theme.generalAnchorMargin
				wrapMode:			Text.WrapAtWordBoundaryOrAnywhere
				text:				installProgressItem.installText
			}
		}

		FilterButton
		{
			id:					closeButton
			anchors.left:		parent.left
			anchors.right:		parent.right
			anchors.bottom:		parent.bottom
			anchors.margins:	Theme.generalAnchorMargin
			visible:			dynamicModules.currentInstallDone
			height:				dynamicModules.currentInstallDone ? implicitHeight : 0

			text:				"Close"
			toolTip:			"Close this window"

			onClicked:			moduleInstallerRect.closeWindow();
		}
	}
}
