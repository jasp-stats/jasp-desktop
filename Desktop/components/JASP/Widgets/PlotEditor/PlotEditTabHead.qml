import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import JASP.Widgets			as	JASPW
import JASP.Theme
import JASP.Controls		as	JASPC
import JASP.PlotEditor

TabButton
{
	id:			refTab
	height:		axes.tabBarHeight + 2
	width:		textItem.implicitWidth + 2 * jaspTheme.generalMenuMargin
	clip:		true
	
	property string buttonText:	"Fill me"
	
	background: Rectangle
	{
		color:			refTab.checked ? jaspTheme.uiBackground : jaspTheme.grayLighter
		radius:			axes.tabButtonRadius
		border.width:	1
		border.color:	refTab.checked ? jaspTheme.uiBorder : jaspTheme.borderColor
		height:			axes.tabBarHeight + axes.tabButtonRadius
		
		
		Rectangle
		{
			color:				jaspTheme.uiBorder
			height:				1
			visible:			!refTab.checked
			anchors
			{
				left:			parent.left
				right:			parent.right
				top:			parent.top
				topMargin:		axes.tabBarHeight + 1
			}
		}
	}

	contentItem: Text
	{
		// The bottom of buttons are hidden to remove their bottom line with the radius
		// So the text has to be moved higher from the horizontal middle line.
		id:					textItem
		topPadding:			-axes.tabButtonRadius * 3/4
		font:				jaspTheme.font
		color:				jaspTheme.black
		horizontalAlignment: Text.AlignHCenter
		verticalAlignment:	Text.AlignVCenter
		opacity:			refTab.checked ? 1 : .6
		text:				buttonText
	}

	MouseArea
	{
		anchors.fill	: parent
		cursorShape		: refTab.checked ? Qt.ArrowCursor : Qt.PointingHandCursor
		acceptedButtons	: Qt.NoButton
	}
}
