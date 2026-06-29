import QtQuick
import QtQuick.Controls
import JASP.Controls
import QtQuick.Effects

DropArea
{
						id:					groupHeaderDropArea
						anchors.left:		parent.left
						anchors.right:		parent.right
						height:				draggableGroup.height + 1
						keys:				["analysis"]

	property int		myIndex:			-1
	property int		myGroupId:			-1
	property string		myGroupTitle:		""
	property bool		myGroupCollapsed:	false
	property var		backgroundFlickable: null

	onEntered: (drag) =>
	{
		if (drag.source.myIndex !== myIndex)
		{
			drag.source.droppedIndex = myIndex
			analysesModel.move(drag.source.myIndex, myIndex)
		}
	}

	Rectangle
	{
		id:					bottomLine
		anchors.bottom:		parent.bottom
		anchors.left:		parent.left
		height:				1
		width:				parent.width + 1
		color:				jaspTheme.buttonBorderColor
		visible:			draggableGroup.state !== "dragging"
	}

	Item
	{
		id:					draggableGroup
		height:				groupRect.height
		activeFocusOnTab:	true

		property int		myIndex:		groupHeaderDropArea.myIndex
		property int		droppedIndex:	-1

		Drag.keys:			["analysis"]
		Drag.active:		groupMouseArea.drag.active
		Drag.hotSpot.x:		width / 2
		Drag.hotSpot.y:		height / 2

		states:
		[
			State
			{
				name: "dragging"
				when: draggableGroup.Drag.active

				ParentChange
				{
					target:			draggableGroup
					parent:			backgroundFlickable
				}

				AnchorChanges
				{
					target:			draggableGroup
					anchors.top:	undefined
					anchors.left:	undefined
					anchors.right:	undefined
				}
			},

			State
			{
				name: "chilling"
				when: !draggableGroup.Drag.active

				ParentChange
				{
					target:			draggableGroup
					parent:			groupHeaderDropArea
				}

				AnchorChanges
				{
					target:			draggableGroup
					anchors.top:	parent.top
					anchors.left:	parent.left
					anchors.right:	parent.right
				}
			}
		]

		RectangularShadow
		{
			anchors.centerIn:	draggableGroup
			width:				draggableGroup.width
			height:				draggableGroup.height
			visible:			draggableGroup.Drag.active
			color:				jaspTheme.grayDarker
			blur:				10
			spread:				3
			radius:				groupRect.radius
			offset.x:			0
			offset.y:			0
		}

		Rectangle
		{
			// Top border line.
			anchors { top: parent.top; topMargin: -1; left: parent.left }
			height:	1
			width:	parent.width
			color:	jaspTheme.buttonBorderColor
		}

		Rectangle
		{
			id:					groupRect
			anchors.top:		parent.top
			anchors.left:		parent.left
			anchors.right:		parent.right
			height:				jaspTheme.formExpanderHeaderHeight + 2 * jaspTheme.formMargin
			color:				jaspTheme.blueMuchLighter
			z:					1

			MouseArea
			{
				id:				groupMouseArea
				hoverEnabled:	true
				cursorShape:	draggableGroup.Drag.active ? Qt.ClosedHandCursor : Qt.PointingHandCursor
				drag.target:	draggableGroup

				drag.onActiveChanged:
				{
					if (drag.active)
					{
						analysesModel.moving		= true
						draggableGroup.droppedIndex	= -1
					}
					else
					{
						analysesModel.moving = false
					}
				}

				onClicked: analysesModel.toggleGroupCollapsed(myGroupId)

				anchors { fill: parent }
				ToolTip.text:	myGroupCollapsed ? qsTr("Click to expand, drag to reorder") : qsTr("Click to collapse, drag to reorder")
				ToolTip.visible: containsMouse && !analysesModel.moving
			}

			// Collapse/expand arrow.
			Image
			{
				id:				collapseArrow
				source:			draggableGroup.activeFocus
								? jaspTheme.iconPath + "large-arrow-right-selected.png"
								: jaspTheme.iconPath + "large-arrow-right.png"
				height:			jaspTheme.formExpanderHeaderHeight * 0.45
				width:			height
				rotation:		myGroupCollapsed ? 0 : 90
				anchors
				{
					left:			parent.left
					leftMargin:		10 * preferencesModel.uiScale
					verticalCenter:	parent.verticalCenter
				}
				sourceSize { width: collapseArrow.width * 2; height: collapseArrow.height * 2 }
				fillMode: Image.PreserveAspectFit

				Behavior on rotation
				{
					enabled: preferencesModel.animationsOn
					RotationAnimation { duration: 200 }
				}
			}

			// Folder icon.
			Image
			{
				id:				folderIcon
				source:			jaspTheme.iconPath + "folder.svg"
				width:			jaspTheme.formExpanderHeaderHeight * 0.45
				height:			width
				anchors
				{
					left:			collapseArrow.right
					leftMargin:		4 * preferencesModel.uiScale
					verticalCenter:	parent.verticalCenter
				}
				sourceSize { width: folderIcon.width * 2; height: folderIcon.height * 2 }
				fillMode:		Image.PreserveAspectFit
			}

			// Editable group title.
			Item
			{
				id:				groupTitleArea
				anchors
				{
					left:			folderIcon.right
					right:			deleteGroupButton.left
					leftMargin:		6 * preferencesModel.uiScale
					rightMargin:	4 * preferencesModel.uiScale
					verticalCenter:	parent.verticalCenter
				}
				height:			groupTitleText.height

				Text
				{
					id:					groupTitleText
					text:				myGroupTitle
					font:				jaspTheme.fontLabel
					color:				jaspTheme.textEnabled
					elide:				Text.ElideMiddle
					visible:			!groupTitleInput.visible
					anchors { left: parent.left; right: parent.right; verticalCenter: parent.verticalCenter }
				}

				TextInput
				{
					id:					groupTitleInput
					font:				jaspTheme.fontLabel
					visible:			false
					selectByMouse:		true
					color:				jaspTheme.grayDarker
					clip:				true
					anchors { left: parent.left; right: parent.right; verticalCenter: parent.verticalCenter }

					Keys.onEscapePressed:				stopEditing(false)
					Keys.onEnterPressed:				stopEditing(true)
					Keys.onReturnPressed:	(event) =>	stopEditing(true)
					onActiveFocusChanged:	if (!activeFocus && visible) stopEditing(true)

					function startEditing()
					{
						text	= myGroupTitle
						visible = true
						forceActiveFocus()
					}

					function stopEditing(store)
					{
						if (store && text.length > 0)
							analysesModel.setGroupTitle(myGroupId, text)
						visible = false
					}
				}

				// Double-click to edit.
				MouseArea
				{
					anchors.fill:	parent
					onDoubleClicked:	groupTitleInput.startEditing()
					cursorShape:		Qt.IBeamCursor
				}
			}

			// Delete group button.
			MenuButton
			{
				id:				deleteGroupButton
				width:			height
				iconSource:		jaspTheme.iconPath + "close-button.png"
				opacity:		0.7
				radius:			height
				toolTip:		qsTr("Remove this group (analyses are kept)")
				onClicked:		analysesModel.removeGroup(myGroupId)
				anchors
				{
					top:			parent.top
					right:			parent.right
					bottom:			parent.bottom
					topMargin:		4 * preferencesModel.uiScale
					bottomMargin:	4 * preferencesModel.uiScale
					rightMargin:	4 * preferencesModel.uiScale
				}
			}
		}
	}
}
