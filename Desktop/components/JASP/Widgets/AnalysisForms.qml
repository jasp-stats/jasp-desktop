import QtQuick
import QtQuick.Controls
import JASP
import JASP.Controls


FocusScope
{
	id:				analysisFormsFocusScope
	implicitWidth:	analysesModel.visible ? jaspTheme.formWidth + 1 + (2 * formsBackground.border.width) + verticalScrollbar.visibleBreadth : 0

	Behavior on width { enabled: preferencesModel.animationsOn; PropertyAnimation { duration: jaspTheme.fileMenuSlideDuration; easing.type: Easing.OutCubic  } }


	Rectangle
	{
		id:				formsBackground
	//	z:				0
		color:			jaspTheme.uiBackground
		anchors.fill:	parent

		property real singleButtonHeight: jaspTheme.formExpanderHeaderHeight + 2 * jaspTheme.formMargin + analysesColumn.spacing

		// "New group" button sitting above the scrollable list.
		Rectangle
		{
			id:				addGroupBar
			anchors.top:	parent.top
			anchors.left:	parent.left
			anchors.right:	parent.right
			height:			jaspTheme.formExpanderHeaderHeight * 0.75
			color:			addGroupMouseArea.containsMouse ? jaspTheme.buttonColorHovered : "transparent"
			z:				3

			Image
			{
				id:				addGroupIcon
				source:			jaspTheme.iconPath + "addition-sign-small.svg"
				height:			parent.height * 0.55
				width:			height
				anchors { left: parent.left; leftMargin: 10 * preferencesModel.uiScale; verticalCenter: parent.verticalCenter }
				sourceSize { width: addGroupIcon.width * 2; height: addGroupIcon.height * 2 }
				fillMode:		Image.PreserveAspectFit
			}

			Text
			{
				text:	qsTr("New Group")
				font:	jaspTheme.fontLabel
				color:	jaspTheme.textEnabled
				anchors { left: addGroupIcon.right; leftMargin: 6 * preferencesModel.uiScale; verticalCenter: parent.verticalCenter }
			}

			MouseArea
			{
				id:				addGroupMouseArea
				anchors.fill:	parent
				hoverEnabled:	true
				cursorShape:	Qt.PointingHandCursor
				onClicked:		analysesModel.addGroup()
				ToolTip.text:	qsTr("Add a group to organise your analyses")
				ToolTip.visible: containsMouse
			}

			Rectangle
			{
				anchors.bottom:	parent.bottom
				anchors.left:	parent.left
				height:			1
				width:			parent.width
				color:			jaspTheme.buttonBorderColor
			}
		}

		Item
		{
			id:				scrollAnalyses
			visible:		analysisFormsFocusScope.width > 0
			z:				2
			clip:			true

			anchors
			{
				top:		addGroupBar.bottom
				left:		parent.left
				right:		parent.right
				bottom:		parent.bottom
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
					//fill:			parent
					//rightMargin:	verticalScrollbar.width
					top:			parent.top
					left:			parent.left
					right:			verticalScrollbar.left
					bottom:			parent.bottom
				}

				Behavior on contentY
				{
					id:			contentYBehaviour
					enabled:	!(analysesFlickable.flicking || analysesFlickable.moving) && preferencesModel.animationsOn;
					PropertyAnimation { duration: 200; easing.type: Easing.OutQuad;   }
				}


				function scrollToElement(targetItem, margin = 0)
				{
					verticalScrollbar.scrollToElement(targetItem, margin, contentYBehaviour)
				}

				Column
				{
					id:				analysesColumn
					width:			analysesFlickable.width
					spacing:		0

					ALTNavigation.enabled:				true
					ALTNavigation.scopeOnly:			true
					ALTNavigation.strategy:				AssignmentStrategy.INDEXED
					ALTNavigation.requestedPostfix:		"A"

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

						// The delegate is a wrapper Item that clips and animates height.
						// The Loader inside has NO explicit height so its item retains its
						// natural height even while the wrapper is collapsed to 0.
						// This avoids the deadlock where Loader.height:0 → item.height:0
						// → targetHeight:0 → height stays 0 after re-opening.
						delegate: Item
						{
							id:						delegateWrapper
							width:					analysesColumn.width
							clip:					true

							// Mirror model roles as regular properties.
							property int		rowIndex:			index
							property bool		isGroupItem:		model.isGroup
							property var		rowAnalysis:		isGroupItem ? null  : model.analysis
							property int		rowGroupId:			isGroupItem ? model.groupId        : -1
							property string		rowGroupTitle:		isGroupItem ? model.groupTitle     : ""
							property bool		rowGroupCollapsed:	isGroupItem ? model.groupCollapsed : false
							property bool		rowIsVisible:		isGroupItem || model.isVisibleInGroup

							// Animate height: the Loader is unconstrained, so innerLoader.item.height
							// always reflects the item's natural size regardless of our own height.
							height: rowIsVisible ? (innerLoader.item ? innerLoader.item.height : 0) : 0

							Behavior on height
							{
								enabled: preferencesModel.animationsOn
								NumberAnimation { duration: 200; easing.type: Easing.OutQuad }
							}

							Loader
							{
								id:		innerLoader
								width:	delegateWrapper.width
								// height intentionally NOT set — item controls its own height.

								sourceComponent: delegateWrapper.isGroupItem ? groupHeaderComponent : analysisExpanderComponent

								onLoaded:
								{
									item.backgroundFlickable = analysesFlickable
									if (!delegateWrapper.isGroupItem)
										item.myAnalysis = delegateWrapper.rowAnalysis
									else
									{
										item.myGroupId        = delegateWrapper.rowGroupId
										item.myGroupTitle     = delegateWrapper.rowGroupTitle
										item.myGroupCollapsed = delegateWrapper.rowGroupCollapsed
									}
								}
							}

							// Keep item properties in sync when model data or index changes.
							Binding { target: innerLoader.item; property: "myIndex";          value: delegateWrapper.rowIndex;          when: innerLoader.item !== null }
							Binding { target: innerLoader.item; property: "myGroupTitle";     value: delegateWrapper.rowGroupTitle;     when: innerLoader.item !== null && delegateWrapper.isGroupItem }
							Binding { target: innerLoader.item; property: "myGroupId";        value: delegateWrapper.rowGroupId;        when: innerLoader.item !== null && delegateWrapper.isGroupItem }
							Binding { target: innerLoader.item; property: "myGroupCollapsed"; value: delegateWrapper.rowGroupCollapsed; when: innerLoader.item !== null && delegateWrapper.isGroupItem }

							ALTNavigation.enabled:		true
							ALTNavigation.index:		rowIndex
							ALTNavigation.onTagMatch:	{ if (!isGroupItem && innerLoader.item) innerLoader.item.expand(); }
						}
					}

					Component
					{
						id: analysisExpanderComponent
						AnalysisFormExpander {}
					}

					Component
					{
						id: groupHeaderComponent
						AnalysisGroupHeader {}
					}
				}
			}
			
			ScrollMoreIndicator 
			{
				id: 		scrollingGuideBottom
				
				anchors
				{
					left: 	parent.left
					right: 	verticalScrollbar.left
					bottom: parent.bottom
				}
				
				extraSpace: analysesFlickable.contentHeight - (analysesFlickable.contentY + analysesFlickable.height)
			}

			ScrollMoreIndicator 
			{
				id: 		scrollingGuideTop
				anchors
				{
					left: 	parent.left
					right: 	verticalScrollbar.left
					top:	parent.top
				}
				
				upsideDown:	true
				extraSpace: analysesFlickable.contentY
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
