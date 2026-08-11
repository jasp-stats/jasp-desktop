import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import JASP.Widgets
import JASP.Controls
import JASP

Rectangle
{
	Accessible.role:	Accessible.Section
	Accessible.name:	title

	color:			"transparent"
	border.color:	jaspTheme.fileMenuLightBorder
	border.width:	1
	radius:			5 * preferencesModel.uiScale

	width:			parent.width
	implicitHeight:	contentColumn.y + contentColumn.height + jaspTheme.generalAnchorMargin
	height:			implicitHeight
	implicitWidth:	Math.max(
						titleText.anchors.leftMargin + titleText.implicitWidth + jaspTheme.generalAnchorMargin,
						contentColumn.anchors.leftMargin + contentColumn.implicitWidth + jaspTheme.generalAnchorMargin
					)

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
		height:				text !== "" ? implicitHeight : 0
	}

	Column
	{
		id:					contentColumn
		spacing:			jaspTheme.generalAnchorMargin
		width:				parent.width - anchors.leftMargin - jaspTheme.generalAnchorMargin

		anchors
		{
			top:			titleText.bottom
			topMargin:		jaspTheme.generalAnchorMargin

			left:			parent.left
			leftMargin:		(titleText.text !== "" ? jaspTheme.subOptionOffset : 0) + jaspTheme.generalAnchorMargin
		}
	}
}
