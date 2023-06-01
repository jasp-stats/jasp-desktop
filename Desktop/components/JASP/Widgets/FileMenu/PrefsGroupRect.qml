import QtQuick			2.11
import QtQuick.Controls 2.4
import QtQuick.Layouts	1.3
import JASP.Widgets		1.0

import JASP.Controls	1.0
import JASP				1.0

Rectangle
{
	color:			"transparent"
	border.color:	jaspTheme.fileMenuLightBorder
	border.width:	1

	implicitHeight:	contentColumn.y + contentColumn.height + jaspTheme.generalAnchorMargin
	height:			implicitHeight
	implicitWidth:	parent.width - (jaspTheme.generalAnchorMargin * 2)
	width:			implicitWidth
	x:				jaspTheme.generalAnchorMargin
	y:				jaspTheme.generalAnchorMargin


			property alias title:		titleText.text
			property alias spacing:		contentColumn.spacing
	default property alias	content:	contentColumn.children

	Text
	{
		id:					titleText
		font:				jaspTheme.fontPrefOptionsGroupTitle
		anchors.margins:	text !== "" ? jaspTheme.generalAnchorMargin : 0
		anchors.top:		parent.top
		anchors.left:		parent.left
		color:				jaspTheme.textEnabled
		text:				""
		height:				text !== "" ? implicitHeight : 0
	}

	Column
	{
		id:					contentColumn
		spacing:			jaspTheme.generalAnchorMargin

		anchors
		{
			top:			titleText.bottom
			topMargin:		jaspTheme.generalAnchorMargin

			left:			parent.left
			leftMargin:		(titleText.text !== "" ? jaspTheme.subOptionOffset : 0) + jaspTheme.generalAnchorMargin

			right:			parent.right
			rightMargin:	jaspTheme.generalAnchorMargin
		}
	}
}
