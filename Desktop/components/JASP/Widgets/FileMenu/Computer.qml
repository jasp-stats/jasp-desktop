import QtQuick
import QtQuick.Controls as QTC
import QtQuick.Layouts
import JASP.Controls
import JASP.Widgets

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
		activeFocusOnTab:		true

		Keys.onBacktabPressed:	computerList.selectLast()
		Keys.onUpPressed:		computerList.selectLast()
		Keys.onTabPressed:		computerList.selectFirst()
		Keys.onDownPressed:		computerList.selectFirst()

	}

	QTC.ToolSeparator
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
		tabbingEscapes:		true


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
