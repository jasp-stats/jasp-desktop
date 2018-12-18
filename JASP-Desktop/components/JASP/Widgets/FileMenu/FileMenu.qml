import QtQuick 2.0
import QtQuick.Controls 2.2
import QtQuick.Layouts 1.3
import JASP.Theme 1.0

Rectangle 
{	
	id:			fileMenu

	width:		actionMenu.width + locationMenu.width + resourceScreen.width
	height:		Math.min(600, mainWindowRoot.height)
	z:			1
	visible:	width > 0

	property variant actionbuttons:			["Open", "Save","Save As", "Export Results", "Export Data","Sync Data", "Close"]
	property variant resourcesbuttons:		["Recent Files", "Current File", "Computer", "OSF", "Data Library"]
	property int action_button_height:		35
	property int resource_button_height:	1.5 * action_button_height
	property int colWidths:					150
  
    // Left verical tab : Action Menu
	Rectangle {

		id: actionMenu
		color:			"#F4F6F7"
		anchors.left:	parent.left
		width:			fileMenuModel.visible ? fileMenu.colWidths : 0
		height:			parent.height
		border.width:	1
		border.color:	Theme.grayDarker

		Behavior on width { PropertyAnimation { duration: 500; easing.type: Easing.InOutSine  } }

		Column {
			id: fileAction
			anchors.top: parent.top
			anchors.topMargin: 5
			anchors.horizontalCenter: parent.horizontalCenter
			spacing:	4
			width:		parent.width - Theme.generalAnchorMargin

			Repeater {

				model: fileMenu.actionbuttons.length

				Button {
					id: actionButton
					text: fileMenu.actionbuttons[index]

					width:parent.width-6
					height: action_button_height
					anchors.leftMargin: 3
					anchors.left: parent.left
					onClicked: {
						fileMenuModel.fileOperationClicked(index)
					}
					enabled: fileMenuModel.buttonsenabled[index];
				}
			}

			Button {

				id: actionTest
				text: "Test"
				visible: false

				width:parent.width-6
				anchors.leftMargin: 3
				anchors.left: parent.left

				onClicked: {
					for (var i=0; i<fileMenu.actionbuttons.length; i++)
						console.log(fileMenu.actionbuttons[i])

					for (var prop in fileMenu.attributes)
						console.log(prop, "=",fileMenu.attributes[prop])

					//var tt = filemenu.fileoperation
					var tt =  filemenu.Close
					console.log("Value Enum is =", tt)

					for (var i=0; i<filemenu.buttonsenabled.length ;i++)
						console.log(filemenu.buttonsenabled[i])

					filemenu.test()

				}
			}
		}// Column fileAction
	}//Rectangle Action Menu

	// Right verical tab : Browse Menu ////////////////////////////////////////////////////////
	// Location Menu
	Rectangle {
		id:				locationMenu
		color:			"#F4F6F7" //Or Theme.whiteBroken?

		width:			fileMenuModel.visible ? fileMenu.colWidths : 0

		anchors.left:	actionMenu.right
		height:			parent.height

		border.width:	1
		border.color:	Theme.grayDarker

		Behavior on width { PropertyAnimation { duration: 500; easing.type: Easing.InOutSine  } }



		Column {
			id: fileLocation

			anchors.top:				parent.top
			anchors.topMargin:			5
			anchors.horizontalCenter:	parent.horizontalCenter
			spacing:					6
			width:						parent.width - Theme.generalAnchorMargin

			Button {
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


			Button {
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


			Button {
				id:				locationComputer
				text:			fileMenu.resourcesbuttons[2]

				width:			parent.width-6
				height:			resource_button_height

				enabled:		true
				onClicked:		resourceScreen.locationSelected = "computer"

				anchors.leftMargin: 3
				anchors.left:		parent.left

			}

			Button {
				id:		alocationOSF
				text:	fileMenu.resourcesbuttons[3]

				width:	parent.width-6
				height: resource_button_height

				anchors.leftMargin:	3
				anchors.left:		parent.left

				enabled: true
				onClicked: {

					resourceScreen.locationSelected= "osf"
					fileMenuModel.resourceButtonClicked(3)
				}
			}

			Button {
				id:			locationDataLibrary
				text:		fileMenu.resourcesbuttons[4]

				width:		parent.width-6
				height:		resource_button_height

				anchors.leftMargin: 3
				anchors.left:		parent.left

				enabled:	true
				visible:	fileMenuModel.datalibrary_button_visible
				onClicked:	resourceScreen.locationSelected= "datalibrary"
			}
		} //Column Filelocation
	}//Rectangle Location Menu



	focus: true
	//Keys.onSpacePressed: locationMenu.visible = !locationMenu.visible

	Rectangle
	{

		id: resourceScreen

		anchors.left:	locationMenu.right
		width:			aButtonVisible && fileMenuModel.visible ? mainWindowRoot.width - (fileMenu.colWidths * 2) : 0
		height:			parent.height
		visible:		fileMenuModel.visible || width > 0

		border.width: 1
		border.color: Theme.grayDarker

		property bool aButtonVisible:	(recentFiles.visible || currentFile.visible || computer.visible || osf.visible || dataLibrary.visible)

		//Wouldn't it be better to do this with some kind of bitflag? Or an enum in FileMenu.h or anything except for a random string...
		property string locationSelected: ""

		Behavior on width { PropertyAnimation { duration: 500; easing.type: Easing.InOutSine  } }

		onWidthChanged: if(width === 0 && !fileMenuModel.visible) resourceScreen.locationSelected = ""

		RecentFiles{
			id: recentFiles

			anchors.fill: parent
			anchors.margins: 1

			visible: resourceScreen.locationSelected == 'recentfiles'
		}

		CurrentFile{
			id: currentFile

			anchors.fill: parent
			anchors.margins: 1

			visible: resourceScreen.locationSelected == 'currentfile'
		}

		Computer{
			id: computer

			anchors.fill: parent
			anchors.margins: 1

			visible: resourceScreen.locationSelected == 'computer'
		}

		OSF{
			id: osf

			anchors.fill: parent
			anchors.margins: 1

			visible: resourceScreen.locationSelected == 'osf'
		}

		DataLibrary{

			id: dataLibrary

			anchors.fill: parent
			anchors.margins: 1

			visible: resourceScreen.locationSelected == 'datalibrary'
		}
	}  //Rectangle resourceScreen

}
