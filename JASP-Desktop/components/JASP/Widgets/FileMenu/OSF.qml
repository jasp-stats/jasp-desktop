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

import QtQuick 2.0
import QtQuick.Controls 2.2
import JASP.Theme 1.0
import JASP.Widgets 1.0

Item
{
	id	: rect

	property bool loggedin			: fileMenuModel.osf.loggedin
	property bool processing		: fileMenuModel.osf.processing
	property bool showfiledialog	: fileMenuModel.osf.showfiledialog

	MenuHeader
	{
		id				: menuHeader
		headertext		: qsTr("Open Science Framework")

		toolseparator	: !loggedin
	}

	RectangularButton
	{
		id		: logoutButton
		text	: qsTr("Logout")
		visible	: loggedin

		anchors.right		: parent.right
		anchors.top			: parent.top
		anchors.rightMargin	: Theme.generalMenuMargin
		anchors.topMargin	: Theme.generalMenuMargin

		onClicked	:
		{
			fileMenuModel.osf.logoutClicked();
		}
	}

	BreadCrumbs
	{
		id		: osfbreadcrumbs
		model	: fileMenuModel.osf.breadCrumbs
		visible	: loggedin

		width	: rect.width
		height	: loggedin ? 40 : 0

		anchors.top			: menuHeader.bottom
		anchors.topMargin	: 30
		anchors.left		: menuHeader.left
		anchors.right		: parent.right

		onCrumbButtonClicked	:
		{
			fileMenuModel.osf.breadCrumbs.indexChanged(modelIndex);
		}
	}

	ToolSeparator
	{
		id			: firstSeparator
		visible		: loggedin
		orientation	: Qt.Horizontal

		anchors.top		: osfbreadcrumbs.bottom
		anchors.left	: menuHeader.left
		anchors.right	: menuHeader.right
	}

	/////////////////////////// File dialog to save in OSF ////////////////////////////////////
	Item
	{
		id		: fileExportDialog
		visible	: showfiledialog && loggedin
		height	: visible ? 30 : 0

		anchors.left			: menuHeader.left
		anchors.right			: menuHeader.right
		anchors.top				: firstSeparator.bottom
		anchors.topMargin		: Theme.generalMenuMargin
		anchors.bottomMargin	: Theme.generalMenuMargin

		Label
		{
			id 		: saveFilenameLabel
			text	: qsTr("Filename")

			width	: 80
			height	: 30
			color 	: Theme.black
			font	: Theme.font

			anchors.top			: parent.top
			anchors.left		: parent.left
			verticalAlignment	: Text.AlignVCenter
		}

		Rectangle
		{
			id		: saveFilenameInput
			height	: saveFilenameLabel.height
			clip	: true

			color			: Theme.white
			border.width	: filenameText.activeFocus ? 5 : 1
			border.color	: filenameText.activeFocus ? Theme.focusBorderColor : Theme.grayDarker

			anchors
			{
				left		: saveFilenameLabel.right
				leftMargin	: Theme.generalAnchorMargin
				top			: saveFilenameLabel.top
				right		: saveFilenameButton.left
				rightMargin	: Theme.generalAnchorMargin
			}

			TextInput
			{
				id				: filenameText
				selectByMouse	: true
				text			: fileMenuModel.osf.savefilename
				font.pixelSize	: 14

				anchors.fill		: parent
				anchors.leftMargin	: Theme.itemPadding
				verticalAlignment	: Text.AlignVCenter

				onAccepted	:
				{
					fileMenuModel.osf.saveFile(filenameText.text)
				}
			}
		}

		RectangularButton
		{
			id		: saveFilenameButton
			width	: 60
			height	: 30
			text	: qsTr("Save")

			anchors.right		: parent.right
			anchors.top			: parent.top

			onClicked	:
			{
				fileMenuModel.osf.saveFile(filenameText.text)
			}
		}
	}

	ToolSeparator
	{
		id			: secondSeparator
		orientation	: Qt.Horizontal
		visible		: fileExportDialog.visible

		anchors.top			: fileExportDialog.bottom
		anchors.topMargin	: Theme.generalAnchorMargin
		anchors.left		: menuHeader.left
		anchors.right		: menuHeader.right
	}

	RectangularButton
	{
		id		: newDirectoryButton
		text	: qsTr("Create Folder")

		width	: 120
		height	: 30
		visible	: showfiledialog && loggedin && !processing

		anchors.left		: menuHeader.left
		anchors.top			: secondSeparator.bottom
		anchors.topMargin	: Theme.generalAnchorMargin

		onClicked	:
		{
			newDirectoryButton.visible = false
		}
	}

	Item
	{
		id		: folderExportDialog
		visible	: !newDirectoryButton.visible && showfiledialog && loggedin && !processing
		height	: visible ? 30 : 0

		anchors.left		: menuHeader.left
		anchors.right		: menuHeader.right
		anchors.top			: secondSeparator.bottom
		anchors.topMargin	: Theme.generalMenuMargin

		Label
		{
			id 		: saveFoldernameLabel
			text	: qsTr("Foldername")

			width	: 80
			height	: 30
			color 	: Theme.black
			font	: Theme.font

			anchors.top			: parent.top
			anchors.left		: parent.left
			anchors.rightMargin	: Theme.generalAnchorMargin
			verticalAlignment	: Text.AlignVCenter
		}

		Rectangle
		{
			id		: saveFoldernameInput
			height	: saveFoldernameLabel.height
			clip	: true

			color			: Theme.white
			border.width	: foldernameText.activeFocus ? 5 : 1
			border.color	: foldernameText.activeFocus ? Theme.focusBorderColor : Theme.grayDarker

			anchors
			{
				left		: saveFoldernameLabel.right
				leftMargin	: Theme.generalAnchorMargin
				top			: saveFoldernameLabel.top
				right		: saveFoldernameButton.left
				rightMargin	: Theme.generalAnchorMargin
			}

			TextInput
			{
				id				: foldernameText
				selectByMouse	: true
				text			: fileMenuModel.osf.savefilename
				font.pixelSize	: 14

				anchors.fill		: parent
				anchors.leftMargin	: Theme.itemPadding
				anchors.rightMargin	: Theme.itemPadding
				verticalAlignment	: Text.AlignVCenter

				onAccepted	:
				{
					fileMenuModel.osf.saveFile(foldernameText.text)
				}
			}
		}

		RectangularButton
		{
			id		: saveFoldernameButton
			width	: 30
			height	: 30
			text	: "+"

			anchors.top			: parent.top
			anchors.right		: cancelCreateFolderButton.left
			anchors.rightMargin : Theme.generalAnchorMargin

			onClicked	:
			{
				fileMenuModel.osf.newFolderClicked()
			}
		}

		RectangularButton
		{
			id		: cancelCreateFolderButton
			width	: 30
			height	: 30
			text	: "x"

			anchors.top			: parent.top
			anchors.right		: parent.right

			onClicked	:
			{
				foldernameText.clear()
				newDirectoryButton.visible = true;
			}
		}
	}

	Item
	{
		visible	: processing
		width	: animation.width;
		height	: animation.height + 8

		anchors.horizontalCenter	: osfList.horizontalCenter
		anchors.verticalCenter		: osfList.verticalCenter

		AnimatedImage
		{
			id		: animation
			source	: "qrc:/icons/loading.gif"
		}
	}

	FileList
	{
		id				: osfList

		visible			: loggedin && !processing
		cppModel		: fileMenuModel.osf.listModel
		hasBreadCrumbs	: true

		anchors
		{
			top				: fileExportDialog.visible ? (newDirectoryButton.visible ? newDirectoryButton.bottom : folderExportDialog.bottom) :  firstSeparator.bottom
			bottom			: parent.bottom
			left			: menuHeader.left
			right			: menuHeader.right
			topMargin		: Theme.generalMenuMargin
			bottomMargin	: Theme.generalMenuMargin
		}
	}

	OSFLogin
	{
		id		: osfLogin
		visible	: !loggedin && !processing

		anchors.horizontalCenter	: parent.horizontalCenter
		anchors.top					: secondSeparator.bottom
		anchors.topMargin			: 40
	}
}
