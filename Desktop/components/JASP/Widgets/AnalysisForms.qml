import QtQuick			2.11
import QtQuick.Controls	2.4
import JASP.Widgets		1.0
import JASP.Controls	1.0


FocusScope
{
	id:				analysisFormsFocusScope
	implicitWidth:	analysesModel.visible ? jaspTheme.formWidth + 1 + (2 * formsBackground.border.width) + verticalScrollbar.visibleBreadth : 0
	width:			implicitWidth

	Behavior on width { enabled: preferencesModel.animationsOn; PropertyAnimation { duration: jaspTheme.fileMenuSlideDuration; easing.type: Easing.OutCubic  } }


	Rectangle
	{
		id:				formsBackground
	//	z:				0
		color:			jaspTheme.uiBackground
		anchors.fill:	parent

		property real singleButtonHeight: jaspTheme.formExpanderHeaderHeight + 2 * jaspTheme.formMargin + analysesColumn.spacing

		Item
		{
			id:				scrollAnalyses
			visible:		analysisFormsFocusScope.width > 0
			z:				2
			clip:			true

			anchors
			{
				top:		parent.top
				left:		parent.left
				right:		parent.right
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
				width:			parent.width - verticalScrollbar.breadth

				anchors
				{
					//fill:			parent
					//rightMargin:	verticalScrollbar.width
					top:			parent.top
					left:			parent.left
					bottom:			parent.bottom
					margins:		formsBackground.border.width
				}

				Behavior on contentY
				{
					id:			contentYBehaviour
					enabled:	!(analysesFlickable.flicking || analysesFlickable.moving) && preferencesModel.animationsOn;
					PropertyAnimation { duration: 200; easing.type: Easing.OutQuad;   }
				}


				function scrollToElement(targetItem, margin = 0)
				{
					const coordinates = targetItem.mapToItem(scrollAnalyses, 0, 0);
					const diffYBottom = coordinates.y + Math.min(targetItem.height, scrollAnalyses.height) - scrollAnalyses.height; //positive if not visible
					const diffYTop = coordinates.y; //negative if not visible

					//check if the object is visisble in the scrollAnalyses (with margin) and scroll to it if not
					if(contentYBehaviour.animation && contentYBehaviour.animation.running)
						return;

					if (diffYBottom > -margin) // scroll down
						analysesFlickable.contentY = analysesFlickable.contentY + Math.max(0, diffYBottom + margin);
					else if (diffYTop < margin) //scroll up
						analysesFlickable.contentY = Math.max(0, analysesFlickable.contentY + Math.min(0, diffYTop - margin));

				}

				Column
				{
					id:				analysesColumn
					width:			analysesFlickable.width
					spacing:		0

					move: Transition
					{
						// This animation may interfere during the Analysis expanding animation.
						// So ensure that it is enabled only when an analysis is dragging
						enabled:	analysesModel.moving && preferencesModel.animationsOn
						NumberAnimation { properties: "y"; easing.type: Easing.OutQuad }
					}

					Repeater
					{
						id:			formRepeater
						model:		analysesModel

						delegate: AnalysisFormExpander
						{
							myIndex:				index
							myAnalysis:				model.analysis
							backgroundFlickable:	analysesFlickable
						}
					}
				}
			}

			MouseArea
			{
				id:					catchMouseEvents
				z:					-10
				onWheel:			(wheel)=>{ wheel.accepted = true; }
				onPositionChanged:	(mouse)=>{ mouse.accepted = true; }
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
