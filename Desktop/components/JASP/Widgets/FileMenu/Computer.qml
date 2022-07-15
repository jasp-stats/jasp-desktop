import QtQuick 2.9
import QtQuick.Controls 2.2
import QtQuick.Layouts 1.3

import JASP.Controls 1.0
import JASP.Widgets 1.0

Item
{
	id:						rect
	focus:					true
	onActiveFocusChanged:	if(activeFocus) computerList.forceActiveFocus()

	MenuHeader
	{
		id:					menuHeader
		headertext:			qsTr("Recent Folders")
		toolseparator:		false
	}

	RoundedButton
	{
		id:						browseButton
		text:					qsTr("Browse")
		anchors.left:			menuHeader.left
		anchors.top:			menuHeader.bottom
		onClicked:				fileMenuModel.computer.browseMostRecent();

		Keys.onBacktabPressed:	computerList.selectLast()
		Keys.onUpPressed:		computerList.selectLast()
		Keys.onTabPressed:		computerList.selectFirst()
		Keys.onDownPressed:		computerList.selectFirst()

	}

	ToolSeparator
	{
		id:					firstSeparator

		anchors.top:		browseButton.bottom
		anchors.left:		menuHeader.left
		anchors.right:		menuHeader.right
		anchors.topMargin:	8 * preferencesModel.uiScale
		width:				rect.width
		orientation:		Qt.Horizontal
	}

	FileList
	{
		id:					computerList
		cppModel:			fileMenuModel.computer.listModel


		anchors
		{
			top:			firstSeparator.bottom
			left:			menuHeader.left
			right:			menuHeader.right
			bottom:			parent.bottom
			topMargin:		jaspTheme.generalMenuMargin
			bottomMargin:	jaspTheme.generalMenuMargin
		}

		KeyNavigation.tab:		browseButton
		KeyNavigation.backtab:	browseButton
	}

}
