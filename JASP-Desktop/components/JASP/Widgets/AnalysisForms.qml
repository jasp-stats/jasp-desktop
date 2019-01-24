import QtQuick			2.11
import QtQuick.Controls	2.4
import JASP.Widgets		1.0
import JASP.Controls	1.0
import JASP.Theme		1.0

FocusScope
{
	id:			analysisFormsFocusScope
	width:		extraSpace + (analysesModel.visible ? Theme.formWidth + 1 + (2 * formsBackground.border.width) + verticalScrollbar.width : 0)

	property int	extraSpace:	analysesModel.count > 0 ? openCloseButton.width : 0

	Behavior on width { PropertyAnimation { duration: Theme.fileMenuSlideDuration; easing.type: Easing.OutCubic  } }

	Rectangle
	{
		id:				formsBackground
		z:				0
		color:			Theme.uiBackground
		border.color:	Theme.uiBorder
		border.width:	1
		//visible:		analyses.count > 0
		anchors.fill:	parent

		/*Item
		{
			anchors.centerIn:	parent
			width:				messageDialog.width
			height:				messageDialog.height
			MessageDialog
			{
				id: messageDialog
				title: "Error"
			}
		}*/

		Rectangle
		{
			id:				openCloseButton
			width:			Theme.splitHandleWidth
			color:			mouseArea.containsMouse ? Theme.grayLighter : Theme.uiBackground
			border.color:	Theme.uiBorder
			border.width:	1
			anchors
			{
				top:		parent.top
				right:		parent.right
				bottom:		parent.bottom
			}

			Image
			{

				readonly property string iconsFolder:		"qrc:/images/"
				readonly property string expandedIcon:		"arrow-left.png"
				readonly property string contractedIcon:	"arrow-right.png"

				source:				iconsFolder + (analysesModel.visible ? expandedIcon : contractedIcon)
				width:				parent.width - 4
				height:				width
				sourceSize.width:	width * 2;
				sourceSize.height:	height * 2;

				anchors.centerIn:	parent

				ToolTip.text:				(analysesModel.visible ? "Hide " : "Show ") + "analysis options"
				ToolTip.timeout:			Theme.toolTipTimeout
				ToolTip.delay:				Theme.toolTipDelay
				ToolTip.toolTip.font:		Theme.font
				ToolTip.visible:			mouseArea.containsMouse
				ToolTip.toolTip.background: Rectangle { color:	Theme.tooltipBackgroundColor }
			}

			MouseArea
			{
				id:				mouseArea
				anchors.fill:	parent
				hoverEnabled:	true
				cursorShape:	Qt.PointingHandCursor
				onClicked:		analysesModel.visible = !analysesModel.visible
				z:				3
			}
		}

		Item
		{
			id:				scrollAnalyses
			visible:		analysisFormsFocusScope.width > analysisFormsFocusScope.extraSpace
			z:				2

			anchors
			{
				top:		parent.top
				left:		parent.left
				right:		openCloseButton.left
				bottom:		parent.bottom
				margins:	parent.border.width
			}

			JASPScrollBar
			{
				id:				verticalScrollbar
				vertical:		true
				flickable:		analysesFlickable
				manualAnchor:	true
				anchors
				{
					top:		parent.top
					right:		parent.right
					bottom:		parent.bottom
				}
			}

			Flickable
			{
				id:				analysesFlickable
				contentWidth:	analysesColumn.width
				contentHeight:	analysesColumn.height

				anchors
				{
					fill:			parent
					rightMargin:	verticalScrollbar.width
				}



				Connections
				{
					target:							analysesModel
					onAnalysisSelectedIndexResults:
						if(row > -1)
						{
							var heightImplodedButton		= (Theme.formExpanderHeaderHeight + analysesColumn.spacing + (Theme.formMargin * 2))
							var previousChildBottomButton	= row <= 0 ? 0 : row * heightImplodedButton
							analysesFlickable.contentY		=  (previousChildBottomButton + analysesModel.currentFormHeight < analysesFlickable.height) ? 0 : previousChildBottomButton
						}

				}

				Column
				{
					id:				analysesColumn
					width:			analysesFlickable.width
					spacing:		1

					Repeater
					{
						model:		analysesModel

						delegate: AnalysisFormExpander
						{
							myIndex:			index
							myID:				analysisID
							analysisTitle:		name
							myAnalysis:         analysis
							formQmlUrl:			formPath
						}
					}
				}
			}
		}
	}

	MouseArea
	{
		id:				catchMouseEvents
		z:				-10
		onWheel:		wheel.accepted = true
		anchors.fill:	parent
	}
}
