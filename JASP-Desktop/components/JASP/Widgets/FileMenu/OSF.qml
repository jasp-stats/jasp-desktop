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

    MenuHeader {
        id: menuHeader

        headertext: qsTr("Open Science Framework")
		toolseparator: !loggedin
    }

    RectangularButton {
        id: logoutButton

        visible: loggedin

        text: qsTr("Logout")

        anchors.right:			parent.right
        anchors.top:			parent.top
        anchors.rightMargin:	Theme.generalMenuMargin
        anchors.topMargin:		Theme.generalMenuMargin

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
        anchors.top:		menuHeader.bottom
        anchors.left:		menuHeader.left
        anchors.right:		parent.right

        onCrumbButtonClicked: fileMenuModel.osf.breadCrumbs.indexChanged(modelIndex);
    }

    ToolSeparator
    {
        id: firstSeparator

        anchors.top: osfbreadcrumbs.bottom
        anchors.left: menuHeader.left
        anchors.right: menuHeader.right
        orientation: Qt.Horizontal

		visible: loggedin
    }

    Item  /////////////////////////// File dialog to save in OSF ////////////////////////////////////
    {
        id: fileExportDialog

        anchors.left: menuHeader.left
        anchors.right: menuHeader.right
        anchors.top: firstSeparator.bottom
        anchors.topMargin:	Theme.generalMenuMargin

        visible: showfiledialog && loggedin
        height: visible ? 90 : 0

        Label {
            id : saveFilenameLabel

            width:	80
            height: 30
            anchors.top: parent.top
            anchors.left:	 parent.left

            text :			qsTr("Filename")
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
            }

            height: saveFilenameLabel.height
            clip: true

            color: Theme.white
            border.width: filenameText.activeFocus ? 5 : 1
            border.color: filenameText.activeFocus ? Theme.focusBorderColor : Theme.grayDarker

            TextInput {

                id: filenameText

                anchors.fill: parent
                selectByMouse: true

                text: fileMenuModel.osf.savefilename

                verticalAlignment: Text.AlignVCenter
                font.pixelSize: 14

                onAccepted: {
                    fileMenuModel.osf.saveFile(filenameText.text)
                }
            }
        }

        RectangularButton
        {
            id:						saveFilenameButton

            anchors.right:		parent.right
            anchors.top:			saveFilenameInput.bottom
            anchors.topMargin:	Theme.generalMenuMargin
            width:60
            height: 30

            text:		qsTr("Save")

            onClicked: {
                fileMenuModel.osf.saveFile(filenameText.text)
            }
        }

        RectangularButton {
            id:						newDirectoryButton

            text:					qsTr("New Folder")
            anchors.right:		saveFilenameButton.left
            anchors.top:			saveFilenameButton.top
            anchors.rightMargin:	Theme.generalMenuMargin
            width:120
            height: 30

            onClicked: {
                fileMenuModel.osf.newFolderClicked()
            }
        }
    }

	ToolSeparator
	{
		id: secondSeparator

		anchors.top: fileExportDialog.bottom
		anchors.left: menuHeader.left
		anchors.right: menuHeader.right
		orientation: Qt.Horizontal
		visible: fileExportDialog.visible
	}
    //////////////////////////////////////////////////////////////////////////////////////


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
			top:		fileExportDialog.visible ? secondSeparator.bottom :  firstSeparator.bottom
			bottom:		parent.bottom
			left:		menuHeader.left
            right:		menuHeader.right
            topMargin:		Theme.generalMenuMargin
            bottomMargin:	Theme.generalMenuMargin
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
