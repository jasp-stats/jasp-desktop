import QtQuick			2.11
import QtQuick.Controls 2.4
import QtQuick.Layouts	1.3
import JASP.Widgets		1.0
import JASP.Theme		1.0
import JASP.Controls	1.0
import JASP				1.0

Rectangle
{
	color:			"transparent"
	border.color:	Theme.fileMenuLightBorder
	border.width:	1

	implicitHeight:	contentColumn.y + contentColumn.height + Theme.generalAnchorMargin
	height:			implicitHeight
	implicitWidth:	parent.width - (Theme.generalAnchorMargin * 2)
	width:			implicitWidth
	x:				Theme.generalAnchorMargin
	y:				Theme.generalAnchorMargin


			property alias title:		titleText.text
			property alias spacing:		contentColumn.spacing
	default property alias	content:	contentColumn.children

	Text
	{
		id:					titleText
		font:				Theme.fontPrefOptionsGroupTitle
		anchors.margins:	text !== "" ? Theme.generalAnchorMargin : 0
		anchors.top:		parent.top
		anchors.left:		parent.left
		text:				""
		height:				text !== "" ? implicitHeight : 0
	}

	Column
	{
		id:					contentColumn
		spacing:			Theme.generalAnchorMargin

		anchors
		{
			top:			titleText.bottom
			topMargin:		Theme.generalAnchorMargin

			left:			parent.left
			leftMargin:		(titleText.text !== "" ? Theme.subOptionOffset : 0) + Theme.generalAnchorMargin

			right:			parent.right
			rightMargin:	Theme.generalAnchorMargin
		}
	}
}
