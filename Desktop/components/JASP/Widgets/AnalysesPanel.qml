import QtQuick
import JASP
import QtQuick.Controls
import JASP.Controls		as JC


Item
{
	id:				analysesPane
	implicitWidth:	analysesForm.width + extraBorder.width
	height:			parent.height

	Rectangle
	{
		// When there is an analysis without data, add an extra border at the left side of the analysis form
		id:						extraBorder
		width:					visible ? jaspTheme.splitHandleWidth : 0
		visible:				!hasData && hasAnalysis && analysesModel.visible
		color:					jaspTheme.uiBackground
		border.width:			1
		border.color:			jaspTheme.uiBorder
		anchors
		{
			top:				parent.top
			bottom:				parent.bottom
			left:				parent.left
			leftMargin:			-1
			topMargin:			-1
			bottomMargin:		-1
		}

	}

	AnalysisForms
	{
		id:						analysesForm
		visible:				hasAnalysis && analysesModel.visible
		width:					hasAnalysis ? implicitWidth : 0
		height:					parent.height
		x:						extraBorder.width
	}
}
