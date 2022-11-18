import QtQuick 2.9
import QtQuick.Controls 2.2
import QtQuick.Layouts 1.3

import JASP.Widgets 1.0
import JASP.Controls 1.0

Item
{

	property bool	toolseparator:	true
	property string headertext:		""
	property string helpfile:		""
	property bool	anchorMe:		true

	function showHelp()
	{
		if(helpfile !== "")
			helpModel.showOrTogglePage(helpfile);
	}

	height:					jaspTheme.menuHeaderHeight
	anchors
	{
		top:				!anchorMe ? undefined : parent.top
		left:				!anchorMe ? undefined : parent.left
		right:				!anchorMe ? undefined : parent.right
		leftMargin:			!anchorMe ? undefined : jaspTheme.generalMenuMargin
		rightMargin:		!anchorMe ? undefined : jaspTheme.generalMenuMargin
	}

	Label
	{
		id:					headLabel
		text:				headertext

		anchors
		{
			verticalCenter:	parent.verticalCenter
			left:			parent.left
		}
		font:				jaspTheme.fontGroupTitle
		color:				jaspTheme.black
	}

	MenuButton
	{
		id:					idHelpButton
		visible:			helpfile !== ""
		height:				parent.height - (jaspTheme.generalAnchorMargin * 2)
		width:				height
		iconSource:			jaspTheme.iconPath + "info-button.png"  // {info-button, info-button-grey}.png Icons made by Freepik from https://www.flaticon.com/
		onClicked:			helpModel.showOrTogglePage(helpfile)
		toolTip:			qsTr("Show info about these preferences")
		radius:				height / 2
		activeFocusOnTab:	true

		anchors
		{
			verticalCenter:	headLabel.verticalCenter
			right:			parent.right
			margins:		jaspTheme.generalAnchorMargin
		}
	}

	ToolSeparator
	{
		id:				firstSeparator

		anchors.bottom:	parent.bottom
		width:			parent.width

		orientation:	Qt.Horizontal
		visible:		toolseparator
	}

}
