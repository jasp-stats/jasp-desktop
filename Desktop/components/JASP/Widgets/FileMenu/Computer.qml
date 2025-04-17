import QtQuick
import QtQuick.Controls
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
	
	
	ScrollMoreIndicator
	{
		anchors
		{
			top:			firstSeparator.bottom
			topMargin:		-firstSeparator.height / 2
			left:			parent.left
			right:			parent.right
		}
		
		upsideDown:	true
		extraSpace:	computerList.contentY
	}
	
	ScrollMoreIndicator
	{
		anchors
		{
			left:			 parent.left
			right:			 parent.right
			bottom:			 parent.bottom
		}
		
		upsideDown:	false
		extraSpace:	computerList.contentHeight - (computerList.contentY + computerList.height)
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
