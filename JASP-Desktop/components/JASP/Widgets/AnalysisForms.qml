import QtQuick			2.11
import QtQuick.Controls	2.4
import JASP.Widgets		1.0
import JASP.Controls	1.0
import JASP.Theme		1.0

FocusScope
{
	id:				analysisFormsFocusScope
	implicitWidth:	extraSpace + (analysesModel.visible ? Theme.formWidth + 1 + (2 * formsBackground.border.width) + Theme.scrollbarBoxWidth : 0)
	width:			implicitWidth

	property int	extraSpace:	analysesModel.count > 0 ? openCloseButton.width : 0

	Behavior on width { PropertyAnimation { duration: Theme.fileMenuSlideDuration; easing.type: Easing.OutCubic  } }


	Rectangle
	{
		id:				formsBackground
	//	z:				0
		color:			Theme.uiBackground
		border.color:	Theme.uiBorder
		border.width:	1
		//visible:		analyses.count > 0
		anchors.fill:	parent

		/*NumberAnimation
		{
			id:				showExpandedForm
			target:			analysesFlickable
			property:		"contentY"
			duration:		200
			easing
			{
				type:		Easing.OutQuad
				amplitude:	3
			}
		}*/

		function getOffset(formIndex) { return formIndex * (Theme.formExpanderHeaderHeight + 2 * Theme.formMargin + analysesColumn.spacing); }

		function scrollToForm(formIndex)
		{
			if (scrollAnalyses.height + getOffset(formIndex) <= analysesFlickable.contentHeight)
			{
				analysesFlickable.contentY = getOffset(formIndex);
			}
			else if (analysesFlickable.contentHeight > scrollAnalyses.height)
			{
				analysesFlickable.contentY = analysesFlickable.contentHeight - scrollAnalyses.height;
			}
		}

		Rectangle
		{
			id:				openCloseButton
			width:			Theme.splitHandleWidth + (2 * border.width)
			height:			parent.height
			//color:			//mouseArea.containsMouse ? Theme.grayLighter : Theme.uiBackground
			border.color:	Theme.uiBorder
			border.width:	1
			anchors.top:	parent.top
			anchors.right:	parent.right


			SplitHandle
			{
				showArrow:				true
				pointingLeft:			analysesModel.visible
				onArrowClicked:			analysesModel.visible = !analysesModel.visible
				anchors
				{
					fill:			parent
					leftMargin:		openCloseButton.border.width
					rightMargin:	openCloseButton.border.width
					//topMargin:		-1
					//bottomMargin:	-1
				}
				toolTipDrag:			mainWindow.dataAvailable	? (mainWindow.dataPanelVisible ? qsTr("Resize data/results")  : qsTr("Drag to show data")) : ""
				toolTipArrow:			analysesModel.visible		? qsTr("Hide input options") : qsTr("Show input options")
				dragEnabled:			mainWindow.dataAvailable && mainWindow.analysesAvailable
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
				bigBar:			true
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
				boundsBehavior: Flickable.StopAtBounds

				anchors
				{
					fill:			parent
					rightMargin:	verticalScrollbar.width
				}

				Behavior on contentY { PropertyAnimation { duration: 200; easing.type: Easing.OutQuad;   } }

				Connections
				{
					target:							analysesModel
					onAnalysisSelectedIndexResults:	reposition();
					onCurrentFormHeightChanged:		reposition();

					function reposition()
					{
						var row = analysesModel.currentAnalysisIndex;

						if(row > -1)
						{
							var heightImplodedButton		= (Theme.formExpanderHeaderHeight + analysesColumn.spacing + (Theme.formMargin * 2))
							var previousChildBottomButton	= row <= 0 ? 0 : row * heightImplodedButton

							//Should we scroll the analysis a bit?
							if(		previousChildBottomButton > analysesFlickable.contentY										// We can actually scroll up a bit if necessary
								||	analysesFlickable.contentY > previousChildBottomButton + analysesModel.currentFormHeight 	// Or the analysis isn't even in view
							)
								analysesFlickable.contentY =  (previousChildBottomButton + analysesModel.currentFormHeight < analysesFlickable.height) ? 0 : previousChildBottomButton
						}
					}

				}

				Column
				{
					id:				analysesColumn
					width:			analysesFlickable.width
					spacing:		1

					Repeater
					{
						id:			formRepeater
						model:		analysesModel

						delegate: AnalysisFormExpander
						{
							myIndex:				index
							myID:					model.analysisID
							myAnalysis:				model.analysis
							formQmlUrl:				model.formPath
							background:				formsBackground
							backgroundFlickable:	analysesFlickable
						}
					}
				}
			}

			MouseArea
			{
				id:					catchMouseEvents
				z:					-10
				onWheel:			wheel.accepted = true
				onPositionChanged:	mouse.accepted = true
				anchors
				{
					fill:			parent
					leftMargin:		-1
					rightMargin:	-1
				}
				hoverEnabled:		true
			}
		}
	}
}
