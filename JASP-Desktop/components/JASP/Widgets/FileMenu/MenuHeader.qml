import QtQuick 2.9
import QtQuick.Controls 2.2
import QtQuick.Layouts 1.3
import JASP.Theme 1.0
import JASP.Widgets 1.0

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

	height:					Theme.menuHeaderHeight
	anchors
	{
		top:				!anchorMe ? undefined : parent.top
		left:				!anchorMe ? undefined : parent.left
		right:				!anchorMe ? undefined : parent.right
		leftMargin:			!anchorMe ? undefined : Theme.generalMenuMargin
		rightMargin:		!anchorMe ? undefined : Theme.generalMenuMargin
	}

	Label
	{
		id:					headLabel
		text:				headertext

		anchors
		{
			verticalCenter:	parent.verticalCenter
			left:			parent.left
			//margins:		Theme.generalAnchorMargin
		}
		font:				Theme.fontGroupTitle
		color:				Theme.black
	}

	MenuButton
	{
		id:					idHelpButton
		visible:			helpfile !== ""
		height:				parent.height - (Theme.generalAnchorMargin * 2)
		width:				height
		iconSource:			"qrc:/images/info-button.png"  // {info-button, info-button-grey}.png Icons made by Freepik from https://www.flaticon.com/
		onClicked:			helpModel.showOrTogglePage(helpfile)
		toolTip:			qsTr("Show info about these preferences")
		radius:				height / 2

		anchors
		{
			verticalCenter:	headLabel.verticalCenter
			right:			parent.right
			margins:		Theme.generalAnchorMargin
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
