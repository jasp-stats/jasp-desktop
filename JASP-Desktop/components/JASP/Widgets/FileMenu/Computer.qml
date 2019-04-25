import QtQuick 2.9
import QtQuick.Controls 2.2
import QtQuick.Layouts 1.3
import JASP.Theme 1.0
import JASP.Widgets 1.0

Item
{
	id:rect

	MenuHeader {
		id: menuHeader

		headertext: qsTr("Recent Folders")
		toolseparator: false
	}

	RectangularButton {
		id: browseButton

		text: qsTr("Browse")
		anchors.left: menuHeader.left
		anchors.top: menuHeader.bottom

		onClicked: {
			fileMenuModel.computer.browseMostRecent();
		}
	}

	ToolSeparator
	{
		id: firstSeparator

		anchors.top: browseButton.bottom
		anchors.left: menuHeader.left
		anchors.right: menuHeader.right
		anchors.topMargin: 8
		width: rect.width
		orientation: Qt.Horizontal
	}

	FileList
	{
		id:				computerList
		cppModel:		fileMenuModel.computer.listModel
		hasBreadCrumbs: false
		focus:			true

		anchors
		{
			top:			firstSeparator.bottom
			left:			menuHeader.left
			right:			menuHeader.right
			bottom:			parent.bottom
			topMargin:		Theme.generalMenuMargin
			bottomMargin:	Theme.generalMenuMargin
		}

	}

}
