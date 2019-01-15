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
	id:		rect

	property bool loggedin:			fileMenuModel.osf.loggedin
	property bool processing:		fileMenuModel.osf.processing
	property bool showfiledialog:	fileMenuModel.osf.showfiledialog


	Label
	{
		id:		headLabel

		width:		implicitWidth
		height:		30
		visible:	!osfLogin.visible
		anchors
		{
			top:		parent.top
			left:		parent.left
			leftMargin: 12
			topMargin:	12
			right:		parent.right
		}
		verticalAlignment: Text.AlignVCenter

		text: "Open Science Framework"
		font: Theme.fontLabel
		color: Theme.black
	}


	RectangularButton {
		id: logoutButton

		visible: loggedin

		text: "Logout"

		anchors.right:			parent.right
		anchors.top:			parent.top
		anchors.rightMargin:	12
		anchors.topMargin:		12

		onClicked: {
			fileMenuModel.osf.logoutClicked();
		}
	}

	BreadCrumbs
	{
		id:		osfbreadcrumbs

		visible: loggedin

		model : fileMenuModel.osf.breadCrumbs

		width:				rect.width
		height:				loggedin ? 40 : 0
		anchors.top:		headLabel.bottom
		anchors.left:		parent.left
		anchors.right:		parent.right
		anchors.leftMargin:	12  //Position datalibrary breadcrumbs

		onCrumbButtonClicked: fileMenuModel.osf.breadCrumbs.indexChanged(modelIndex);
	}



	Item  /////////////////////////// File dialog to save in OSF ////////////////////////////////////
	{

		id: fileExportDialog

		width: rect.width
		visible: showfiledialog
		anchors.top: osfbreadcrumbs.bottom
		anchors.topMargin: 6
		height: visible ? 90 : 0


		ToolSeparator
		{
			id: firstSeparator
			anchors.top: fileExportDialog.top
			width: rect.width
			orientation: Qt.Horizontal
		}

		Label {
			id : saveFilenameLabel

			width:	80
			height: 30
			anchors
			{
				top:		firstSeparator.bottom
				left:		parent.left
				leftMargin: 12
				topMargin:	6
				right:		parent.right
			}

			text :			"Filename"
			font.family:	"SansSerif"
			font.pixelSize: 14
			color:			Theme.black
			verticalAlignment: Text.AlignVCenter
		}

		Rectangle
		{

			id: saveFilenameInput

			anchors
			{
				left: saveFilenameLabel.right
				leftMargin: 6
				top: saveFilenameLabel.top
				right: parent.right
				rightMargin: 12
			}
			height: saveFilenameLabel.height
			clip: true

			color: Theme.white
			border.width: filenameText.activeFocus ? 5 : 1
			border.color: filenameText.activeFocus ? Theme.focusBorderColor : Theme.grayDarker

			TextInput {

				id: filenameText

				anchors.fill: parent
				anchors.leftMargin: 10
				selectByMouse: true

				text: fileMenuModel.osf.savefilename

				verticalAlignment: Text.AlignVCenter
				font.pixelSize: 14

				onAccepted: {
					fileMenuModel.osf.saveFile(filenameText.text)
				}
			}
		}

		RectangularButton {
			id:						newDirectoryButton

			text:					"New Folder"
			anchors.right:			saveFilenameButton.left
			anchors.top:			saveFilenameInput.bottom
			anchors.rightMargin:	12
			anchors.topMargin:		12

			onClicked: {
				fileMenuModel.osf.newFolderClicked()
			}
		}

		RectangularButton {
			id:						saveFilenameButton

			text:					"Save"
			anchors.right:			parent.right
			anchors.top:			newDirectoryButton.top
			anchors.rightMargin:	12

			onClicked: {
				fileMenuModel.osf.saveFile(filenameText.text)
			}
		}
	}
	//////////////////////////////////////////////////////////////////////////////////////

	ToolSeparator
	{
		id: secondSeparator
		anchors.top: fileExportDialog.bottom
		width: rect.width
		orientation: Qt.Horizontal

		// TODO: remove this and headLabel
		visible: !osfLogin.visible
	}

	Item {

		visible: processing

		width: animation.width;
		height: animation.height + 8

		anchors.horizontalCenter: osfList.horizontalCenter
		anchors.verticalCenter: osfList.verticalCenter

		AnimatedImage { id: animation; source: "qrc:/icons/loading.gif" }
	}

	FileList {
		id:			osfList
		visible:	loggedin && !processing
		cppModel:	fileMenuModel.osf.listModel
		hasBreadCrumbs : true
		anchors
		{
			top:			secondSeparator.bottom
			left:			parent.left
			right:			parent.right
			bottom:			parent.bottom
			leftMargin:		12  //Position datalibrary items
			topMargin:		Theme.generalAnchorMargin
			bottomMargin:	Theme.generalAnchorMargin
			rightMargin:	Theme.generalAnchorMargin
		}
	}

	OSFLogin {
		id: osfLogin

		visible: !loggedin && !processing

		anchors.horizontalCenter: parent.horizontalCenter
		anchors.top             : secondSeparator.bottom
        anchors.topMargin       : 40
	}
}
