import QtQuick			2.11
import QtQuick.Controls 2.4
import JASP.Controls	1.0

import QtGraphicalEffects 1.12

DropArea
{
	id:					analysisFormExpander
	anchors.left:		parent.left
	anchors.right:		parent.right
	height:				expanderButton.height + 1
	keys:				["analysis"]

	property alias		myIndex:			draggableItem.myIndex
	property alias		myAnalysis:         loader.myAnalysis
	property alias		formQmlUrl:			loader.formQmlUrl
	property alias		backgroundFlickable:loader.backgroundFlickable

	onEntered:
	{
		if (drag.source.myIndex !== myIndex)
		{
			drag.source.droppedIndex = myIndex
			analysesModel.move(drag.source.myIndex, myIndex)
		}
	}

	function toggleExpander()
	{
		if(analysesModel.currentAnalysisIndex === draggableItem.myIndex)	analysesModel.unselectAnalysis()
		else																analysesModel.selectAnalysisAtRow(draggableItem.myIndex);
	}

	Component.onCompleted: myAnalysis.expandAnalysis.connect(toggleExpander)

	Rectangle
	{
		id:		bottomLine
		anchors
		{
			bottom:	parent.bottom
			left:	parent.left
		}
		height:	1
		width:	parent.width + 1
		color:	jaspTheme.buttonBorderColor
		visible: draggableItem.state != "dragging"
	}

	Item
	{
		id:					draggableItem
		height:				loaderAndError.y
		anchors.top:		parent.top
		anchors.left:		parent.left
		anchors.right:		parent.right

		property int		myIndex:			-1
		property int		droppedIndex:		-1
		property alias		myAnalysis:			loader.myAnalysis

		Drag.keys:			["analysis"]
		Drag.active:		mouseArea.drag.active
		Drag.hotSpot.x:		width/2
		Drag.hotSpot.y:		height/2

		states: [
			State {
				name: "dragging"
				when: draggableItem.Drag.active
				ParentChange {
					target:			draggableItem
					parent:			backgroundFlickable
				}
				PropertyChanges {
					target:			shadow
					visible:		true
				}
				PropertyChanges {
					target:			mouseArea
					cursorShape:	Qt.ClosedHandCursor
				}
				AnchorChanges {
					target:			draggableItem
					anchors.top:	undefined
					anchors.left:	undefined
					anchors.right:	undefined
				}
			}
		]

		ToolTip
		{
			text:			qsTr("Drag to reorder the analyses")
			timeout:		jaspTheme.toolTipTimeout
			delay:			jaspTheme.toolTipDelay
			font:			jaspTheme.font
			background:		Rectangle { color:	jaspTheme.tooltipBackgroundColor }
			visible:		mouseArea.containsMouse && !analysesModel.moving && analysesModel.rowCount() > 1
			y:				mouseArea.mouseY
			x:				mouseArea.mouseX + 5
		}

		MouseArea
		{
			id:				mouseArea
			onClicked:		analysisFormExpander.toggleExpander();
			hoverEnabled:	true
			cursorShape:	Qt.PointingHandCursor
			drag.target:	draggableItem

			drag.onActiveChanged:
			{
				if (drag.active)
				{
					analysesModel.unselectAnalysis()
					analysesModel.moving = true
					draggableItem.droppedIndex = -1
				}
				else
				{
					analysesModel.moving = false
					analysesModel.moveAnalysesResults(loader.myAnalysis, draggableItem.droppedIndex)
				}
			}

			anchors
			{
				top:	parent.top
				left:	parent.left
				right:	parent.right
			}
			height: jaspTheme.formExpanderHeaderHeight + (2 * jaspTheme.formMargin) //We only want to see a tooltip when we are hovering the "button" part of AnalysisFormExpander
		}

		RectangularGlow
		{
			id				: shadow
			anchors.centerIn: draggableItem
			width			: draggableItem.width
			height			: draggableItem.height
			visible			: false
			color			: jaspTheme.grayDarker
			spread			: 0.2
			cornerRadius	: expanderButton.radius + glowRadius
			glowRadius		: 5
		}

		Rectangle
		{
			// This line appears only when the analysis above this one is dragged.
			anchors
			{
				top: parent.top
				topMargin: -1
				left:	parent.left
			}
			height: 1
			width: parent.width
			color: jaspTheme.buttonBorderColor
		}

		Rectangle
		{
			id:					expanderButton
			height:				loaderAndError.y
			anchors.top:		parent.top
			anchors.left:		parent.left
			anchors.right:		parent.right
			z:					shadow.z + 1
			color:				jaspTheme.uiBackground
			clip:				true

			property bool		expanded:			analysesModel.currentAnalysisIndex === myIndex
			property bool		imploded:			height == loader.y
			property real		formHeight:			0

			onFormHeightChanged: if(analysesModel.currentAnalysisIndex === draggableItem.myIndex) analysesModel.currentFormHeight = expanderButton.formHeight

			states: [
				State
				{
					name: "expanded";	when: expanderButton.expanded
					PropertyChanges {	target: expanderButton;		height: loaderAndError.y + loaderAndError.height;	}
				}
			]

			transitions: Transition
			{
				enabled: preferencesModel.animationsOn

				// Do not use a behavior here: this would interfere with the animation of the ExpanderButtons in the form
				NumberAnimation		{ property: "height";	duration: 200 }
			}

			Item
			{
				id:				expanderRectangle
				height:			jaspTheme.formExpanderHeaderHeight  //label.contentHeight

				anchors
				{
					left:		parent.left
					right:		parent.right
					top:		parent.top
					topMargin:	jaspTheme.formMargin
				}

				Image
				{
					id:					expanderIcon
					anchors
					{
						left:			parent.left
						leftMargin:		10 * preferencesModel.uiScale
						verticalCenter:	parent.verticalCenter
					}
					rotation:		expanderButton.expanded ? 90 : 0
					height:			analysisTitle.height * 0.88 //expanderRectangle.height / 1.5
					width:			height
					source:			jaspTheme.iconPath + "/large-arrow-right.png"
					sourceSize
					{
						width:	expanderIcon.width * 2
						height:	expanderIcon.height * 2
					}

					Behavior on rotation { enabled: preferencesModel.animationsOn; RotationAnimation { duration: 200 } }

				}

				Item
				{
					id:			analysisTitleItem
					height:		analysisTitle.height

					anchors
					{
						left:			expanderIcon.right
						right:			editButton.left
						leftMargin:		expanderIcon.anchors.leftMargin
						rightMargin:	2 * preferencesModel.uiScale
						verticalCenter:	parent.verticalCenter
					}

					Text
					{
						id:				analysisTitle
						text:			loader.myAnalysis != null ? loader.myAnalysis.title : "?"
						font:			jaspTheme.fontLabel
						color:			jaspTheme.textEnabled
						visible:		!analysisTitleInput.visible
						elide:			Text.ElideMiddle

						anchors
						{
							left:			parent.left
							right:			parent.right
							verticalCenter:	parent.verticalCenter
						}
					}

					TextInput
					{
						id:					analysisTitleInput
						font:				jaspTheme.fontLabel
						visible:			false
						selectByMouse:		true
						color:				jaspTheme.grayDarker
						clip:				true

						anchors
						{
							left:			parent.left
							right:			parent.right
							verticalCenter:	parent.verticalCenter
						}


						Keys.onEscapePressed: 	stopEditing(false);
						Keys.onEnterPressed:	stopEditing(true);
						Keys.onReturnPressed: 	stopEditing(true);
						onActiveFocusChanged:	if(!activeFocus && visible)	stopEditing(true);

						function startEditing()
						{
							text	= analysisTitle.text;
							visible = true;

							forceActiveFocus();
						}

						function stopEditing(storeChangedValue)
						{
							if(storeChangedValue && loader.myAnalysis != null)
								loader.myAnalysis.title = text;

							visible = false;
						}
					}
				}

				MenuButton
				{
					id:					editButton
					width:				height
					iconSource:			jaspTheme.iconPath + "/edit-pencil.png" // Icon made by Chanut from https://www.flaticon.com/
					enabled:			expanderButton.expanded
					onClicked:			analysisTitleInput.startEditing();
					toolTip:			qsTr("Edit the title of this analysis")
					radius:				height
					opacity:			enabled ? 1 : 0.1
					anchors
					{
						top:			parent.top
						right:			copyButton.left
						bottom:			parent.bottom
						topMargin:		4 * preferencesModel.uiScale
						bottomMargin:	4 * preferencesModel.uiScale
					}
				}

				MenuButton
				{
					id:					copyButton
					width:				height
					iconSource:			enabled ? jaspTheme.iconPath + "/duplicate.png" : jaspTheme.iconPath + "/duplicate_disabled.png"
					enabled:			expanderButton.expanded
					onClicked:			analysisFormExpander.myAnalysis.duplicateMe()
					toolTip:			qsTr("Duplicate this analysis")
					radius:				height
					opacity:			editButton.opacity
					anchors
					{
						top:			parent.top
						right:			helpButton.left
						bottom:			parent.bottom
						topMargin:		editButton.anchors.topMargin
						bottomMargin:	editButton.anchors.bottomMargin
					}
				}

				MenuButton
				{
					id:					helpButton
					width:				height
					iconSource:			enabled ? jaspTheme.iconPath + "info-button.png" : jaspTheme.iconPath + "info-button-black.png" // {info-button, info-button-grey}.png Icons made by Freepik from https://www.flaticon.com/
					opacity:			editButton.opacity
					//visible:			expanderButton.expanded || hovered || mouseArea.containsMouse
					enabled:			expanderButton.expanded
					onClicked:			if(preferencesModel.generateMarkdown)
											helpModel.markdown = Qt.binding(function(){ return myAnalysis.helpMD; });
										else
										{
											helpModel.markdown = ""; //To break any previous binding we might have made
											helpModel.showOrTogglePage(loader.myAnalysis.helpFile)
										}
					toolTip:			qsTr("Show info for this analysis")
					radius:				height
					anchors
					{
						top:			parent.top
						right:			closeButton.left
						bottom:			parent.bottom
						topMargin:		editButton.anchors.topMargin
						bottomMargin:	editButton.anchors.bottomMargin
					}
				}

				MenuButton
				{
					id:					closeButton
					width:				height
					iconSource:			enabled ? jaspTheme.iconPath + "close-button.png" : jaspTheme.iconPath + "close-button-black.png" // {close-button, close-button-grey}.png Icons made by Smashicons from https://www.flaticon.com/
					opacity:			editButton.opacity
					//visible:			expanderButton.expanded || hovered || mouseArea.containsMouse
					enabled:			expanderButton.expanded
					onClicked:			analysesModel.removeAnalysis(loader.myAnalysis)
					toolTip:			qsTr("Remove this analysis")
					radius:				height
					anchors
					{
						top:			parent.top
						right:			parent.right
						bottom:			parent.bottom
						topMargin:		editButton.anchors.topMargin
						bottomMargin:	editButton.anchors.bottomMargin
					}
				}
			}


			Item
			{
				id:			loaderAndError
				height:		Math.max(loader.height, errorRect.height * preferencesModel.uiScale)
				visible:	!expanderButton.imploded

				anchors
				{
					top:				expanderRectangle.bottom
					left:				parent.left
					right:				parent.right
					margins:			jaspTheme.formMargin
				}

				Rectangle
				{
					id: errorRect
					visible:		loader.status === Loader.Error
					anchors.top:	parent.top
					color:			jaspTheme.errorMessagesBackgroundColor
					width:			jaspTheme.formWidth - ( 2 * jaspTheme.formMargin )
					height:			visible ? errorMessagesText.height : 0

					Text
					{
						id:					errorMessagesText
						anchors.centerIn:	parent
						width:				parent.width
						padding:			5
						verticalAlignment:	Text.AlignVCenter
						text:				loader.status === Loader.Error ? loader.sourceComponent.errorString() : ""
						wrapMode:			Text.Wrap
					}
				}

				Connections
				{
					target:				loader.item
					onHeightChanged:	expanderButton.formHeight = loader.item.height
				}

				Loader
				{
					id:					loader
					source:				!expanderButton.imploded || expanderButton.expanded ? loader.formQmlUrl : ""
					asynchronous:		false // makes it slow when true

					anchors
					{
						top:			errorRect.bottom
						topMargin:		errorRect.visible ? jaspTheme.formMargin : 0
						left:			parent.left
						right:			parent.right
					}

					property var myAnalysis:			null
					property var backgroundFlickable:	null
					property string formQmlUrl

					onLoaded:	if(source !== "") analysesModel.currentFormHeight = loader.item.height
				}
			}
		}
	}
}
