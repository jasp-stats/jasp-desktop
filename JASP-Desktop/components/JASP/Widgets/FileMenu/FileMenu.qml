import QtQuick 2.11
import QtQuick.Controls 2.2
import QtQuick.Layouts 1.3
import JASP.Theme 1.0
import JASP.Widgets 1.0

Item
{	
	id:			fileMenu

	width:		slidePart.width
	height:		600
	z:			1
	visible:	actionMenu.x + actionMenu.width > 0

	property variant actionbuttons:			["Open", "Save","Save As", "Export Results", "Export Data","Sync Data", "Close"]
	property variant resourcesbuttons:		["Recent Files", "Current File", "Computer", "OSF", "Data Library"]
	property int action_button_height:		35
	property int resource_button_height:	1.5 * action_button_height
	property int colWidths:					150
  
	Item
	{
		id:		slidePart
		x:		fileMenuModel.visible ? 0 : -(colWidths * 2)
		width:	actionMenu.width + locationMenu.width + resourceScreen.width
		height:	fileMenu.height

		Behavior on x { PropertyAnimation { duration: Theme.fileMenuSlideDuration; easing.type: Easing.OutCubic  } }


		// Left verical tab : Action Menu
		Rectangle {

			id:				actionMenu
			color:			Theme.uiBackground
			anchors.left:	parent.left
			width:			fileMenu.colWidths //fileMenuModel.visible ?  : 0
			height:			parent.height
			border.width:	1
			border.color:	Theme.uiBorder


			Column {
				id: fileAction
				anchors.top: parent.top
				anchors.topMargin: 5
				anchors.horizontalCenter: parent.horizontalCenter
				spacing:	4
				width:		parent.width - Theme.generalAnchorMargin

				Repeater {

					model: fileMenu.actionbuttons.length

					MenuButton {
						id:					actionButton
						text:				fileMenu.actionbuttons[index]

						width:				parent.width-6
						height:				action_button_height
						anchors.leftMargin: 3
						anchors.left:		parent.left
						onClicked:			fileMenuModel.fileOperationClicked(index)
						enabled:			fileMenuModel.buttonsenabled[index];
					}
				}
			}
		}//Rectangle Action Menu


		// Right verical tab : Browse Menu ////////////////////////////////////////////////////////
		// Location Menu
		Rectangle {
			id:				locationMenu
			color:			Theme.uiBackground

			width:			fileMenu.colWidths //fileMenuModel.visible ?  : 0

			anchors.left:	actionMenu.right
			height:			parent.height

			border.width:	1
			border.color:	Theme.uiBorder

			//Behavior on x { PropertyAnimation { duration: Theme.fileMenuSlideDuration; easing.type: Easing.InOutSine  } }

			Column {
				id: fileLocation

				anchors.top:				parent.top
				anchors.topMargin:			5
				anchors.horizontalCenter:	parent.horizontalCenter
				spacing:					6
				width:						parent.width - Theme.generalAnchorMargin

				MenuButton {
					id:					locationRecentFiles
					text:				fileMenu.resourcesbuttons[0]

					width:				parent.width-6
					height:				resource_button_height
					anchors.leftMargin: 3
					anchors.left:		parent.left

					enabled:			true
					visible:			fileMenuModel.recentfiles_button_visible
					onClicked:			resourceScreen.locationSelected = "recentfiles"
				}


				MenuButton {
					id:					locationCurrentFile
					text:				fileMenu.resourcesbuttons[1]

					width:				parent.width-6
					height:				resource_button_height
					anchors.leftMargin: 3
					anchors.left:		parent.left

					enabled:		true
					visible:		fileMenuModel.currentfile_button_visible
					onClicked:		resourceScreen.locationSelected = "currentfile"
				}


				MenuButton {
					id:				locationComputer
					text:			fileMenu.resourcesbuttons[2]

					width:			parent.width-6
					height:			resource_button_height

					enabled:		true
					onClicked:		resourceScreen.locationSelected = "computer"

					anchors.leftMargin: 3
					anchors.left:		parent.left

				}

				MenuButton {
					id:		alocationOSF
					text:	fileMenu.resourcesbuttons[3]

					width:	parent.width-6
					height: resource_button_height

					anchors.leftMargin:	3
					anchors.left:		parent.left

					enabled: true
					onClicked: {

						resourceScreen.locationSelected = "osf"
						fileMenuModel.resourceButtonClicked(3)
					}
				}

				MenuButton {
					id:			locationDataLibrary
					text:		fileMenu.resourcesbuttons[4]

					width:		parent.width-6
					height:		resource_button_height

					anchors.leftMargin: 3
					anchors.left:		parent.left

					enabled:	true
					visible:	fileMenuModel.datalibrary_button_visible
					onClicked:	resourceScreen.locationSelected = "datalibrary"
				}
			}	//Column Filelocation
		}		//Rectangle Location Menu



		focus: true
		//Keys.onSpacePressed: locationMenu.visible = !locationMenu.visible

		Item
		{
			id:			dropShadow
			y:			0
			x:			resourceScreen.x + resourceScreen.width
			height:		resourceScreen.height
			width:		Theme.shadowRadius
			visible:	resourceScreen.visible

			Rectangle
			{
				anchors.centerIn: parent
				rotation:	-90
				gradient:	Gradient {
					GradientStop { position: 0.0; color: Theme.shadow }
					GradientStop { position: 1.0; color: "transparent" } }
				height:		dropShadow.width
				width:		dropShadow.height
			}
		}

		Rectangle
		{
			id: resourceScreen

			property real	otherColumnsWidth: fileMenu.colWidths * 2

			//anchors.left:	locationMenu.right

			x:				otherColumnsWidth - (aButtonVisible && fileMenuModel.visible ? 0 : width)
			width:			Math.min(mainWindowRoot.width - otherColumnsWidth, 800)
			height:			parent.height
			visible:		fileMenuModel.visible || x + width > otherColumnsWidth + 1

			border.width:	1
			border.color:	Theme.grayDarker
			color:			Theme.uiBackground
			z:				-2

			property bool aButtonVisible:	(recentFiles.visible || currentFile.visible || computer.visible || osf.visible || dataLibrary.visible)

			//Wouldn't it be better to do this with some kind of bitflag? Or an enum in FileMenu.h or anything except for a random string...
			property string locationSelected: ""

			Behavior on x { PropertyAnimation { duration: Theme.fileMenuSlideDuration; easing.type: Easing.OutCubic  } }

			onXChanged: if(x + width <= otherColumnsWidth && !fileMenuModel.visible) resourceScreen.locationSelected = ""

			RecentFiles
			{
				id:				recentFiles
				anchors.fill:	parent
				visible:		resourceScreen.locationSelected == 'recentfiles'
			}

			CurrentFile
			{
				id:				currentFile
				anchors.fill:	parent
				visible:		resourceScreen.locationSelected == 'currentfile'
			}

			Computer
			{
				id:				computer
				anchors.fill:	parent
				visible:		resourceScreen.locationSelected == 'computer'
			}

			OSF
			{
				id:				osf
				anchors.fill:	parent
				visible:		resourceScreen.locationSelected == 'osf'
			}

			DataLibrary
			{

				id:				dataLibrary
				anchors.fill:	parent
				visible:		resourceScreen.locationSelected == 'datalibrary'
			}
		}
	}
}
