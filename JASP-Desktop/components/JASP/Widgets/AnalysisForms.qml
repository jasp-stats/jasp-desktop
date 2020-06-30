import QtQuick			2.11
import QtQuick.Controls	2.4
import JASP.Widgets		1.0
import JASP.Controls	1.0


FocusScope
{
	id:				analysisFormsFocusScope
	implicitWidth:	extraSpace + (analysesModel.visible ? jaspTheme.formWidth + 1 + (2 * formsBackground.border.width) + jaspTheme.scrollbarBoxWidth : 0)
	width:			implicitWidth

	property int	extraSpace:	analysesModel.count > 0 ? openCloseButton.width : 0

	Behavior on width { enabled: preferencesModel.animationsOn; PropertyAnimation { duration: jaspTheme.fileMenuSlideDuration; easing.type: Easing.OutCubic  } }


	Rectangle
	{
		id:				formsBackground
	//	z:				0
		color:			jaspTheme.uiBackground
		border.color:	jaspTheme.uiBorder
		border.width:	1
		anchors.fill:	parent

		property real singleButtonHeight: jaspTheme.formExpanderHeaderHeight + 2 * jaspTheme.formMargin + analysesColumn.spacing

		function getOffset(formIndex) { return formIndex < 0 ? 0 : formIndex * singleButtonHeight; }

		function scrollToForm(formIndex)
		{
			if(formIndex < 0) return;

			var offset = getOffset(formIndex);

			if(formIndex === 0)
			{
				analysesFlickable.contentY = 0;
				return;
			}

			if (analysesModel.currentFormHeight + offset + singleButtonHeight <= analysesFlickable.contentHeight || analysesModel.currentFormHeight + singleButtonHeight > scrollAnalyses.height)
			{
				analysesFlickable.contentY = offset;
				return;
			}

			analysesFlickable.contentY = Math.max(0, offset + analysesModel.currentFormHeight + singleButtonHeight - scrollAnalyses.height);

		}

		Connections
		{
			target:							analysesModel
			onCurrentAnalysisIndexChanged:	formsBackground.scrollToForm(analysesModel.currentAnalysisIndex);
		}

		Rectangle
		{
			id:				openCloseButton
			width:			jaspTheme.splitHandleWidth + (2 * border.width)
			height:			parent.height
			//color:			//mouseArea.containsMouse ? jaspTheme.grayLighter : jaspTheme.uiBackground
			border.color:	jaspTheme.uiBorder
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
				clip:			true

				anchors
				{
					fill:			parent
					rightMargin:	verticalScrollbar.width
				}

				Behavior on contentY
				{
					id:			contentYBehaviour
					enabled:	!(analysesFlickable.flicking || analysesFlickable.moving) && preferencesModel.animationsOn;
					PropertyAnimation { duration: 200; easing.type: Easing.OutQuad;   }
				}

				Connections
				{
					target:							analysesModel
					onCurrentFormHeightChanged:		if(analysesModel.currentFormHeight > analysesModel.currentFormPrevH) reposition(); //If it got larger it probably means an expander opened and we should reposition if possible

					function reposition()
					{
						var row = analysesModel.currentAnalysisIndex;

						if(row > -1 && row === analysesModel.currentAnalysisIndex)
						{
							var previousAnalysisButtonBottom	= formsBackground.getOffset(row);

							//Should we scroll the analysis a bit?
							if(		previousAnalysisButtonBottom	> analysesFlickable.contentY										// We can actually scroll up a bit if necessary
								||	analysesFlickable.contentY		> previousAnalysisButtonBottom + analysesModel.currentFormHeight 	// Or the analysis isn't even in view
							)
							{

								if(!contentYBehaviour.animation.running)
									formsBackground.scrollToForm(row);
							}
						}
					}
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
							formQmlUrl:				model.formPath
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
