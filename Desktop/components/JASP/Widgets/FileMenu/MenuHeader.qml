import QtQuick
import QtQuick.Controls as QtC
import QtQuick.Layouts
import JASP.Widgets
import JASP.Controls

Item
{

	Accessible.role:	Accessible.StaticText
	Accessible.name:	headertext

	property bool	toolseparator:	true
	property string headertext:		""
	property string helpfile:		""
	property alias	helpMD:			idHelpButton.helpMD
	property bool	addMargin:		true

	function showHelp()
	{
		if(helpfile !== "")
			helpModel.showOrTogglePage(helpfile);
	}

	x:						addMargin ? jaspTheme.generalMenuMargin : 0
	width:					parent.width - (addMargin ? 2 * jaspTheme.generalMenuMargin : 0)
	height:					jaspTheme.menuHeaderHeight

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

	HelpButton
	{
		id:					idHelpButton
		text:				qsTr("Help")
		visible:			helpfile !== ""
		height:				parent.height - (jaspTheme.generalAnchorMargin * 2)
		width:				height
		buttonPadding:		6 * preferencesModel.uiScale
		toolTip:			qsTr("Show info about these preferences")
		helpPage:			helpfile

		anchors
		{
			verticalCenter:	headLabel.verticalCenter
			right:			parent.right
			margins:		jaspTheme.generalAnchorMargin
		}
	}

	QtC.ToolSeparator
	{
		id:				firstSeparator

		anchors.bottom:	parent.bottom
		width:			parent.width

		orientation:	Qt.Horizontal
		visible:		toolseparator
		padding:		0
	}
	
	property real separatorHalve:	firstSeparator.height / 2

}
