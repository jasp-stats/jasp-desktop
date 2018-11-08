import QtQuick 2.0
import QtQuick.Controls 2.2
import QtQuick.Layouts 1.3

Rectangle 
{	
    id: fileMenu

    width: 600
    height:400

    property variant actionbuttons: ["Open", "Save","Save As", "Export Results", "Export Data","Sync Data", "Close"]
	property variant resourcesbuttons: ["Recent Files", "Current File", "Computer", "OSF", "Data Library"]
	property int action_button_height: 35
	property int resource_button_height: 1.5 * action_button_height
  
    // Left verical tab : Action Menu
    Rectangle {

        id: actionMenu
        color: "#F4F6F7"
        anchors.left: parent.left
        width: 150;
        height: parent.height
        border.width: 1
        border.color: "darkgray"

        Column {
            id: fileAction
            anchors.top: parent.top
            anchors.topMargin: 5
            anchors.horizontalCenter: parent.horizontalCenter
            spacing: 4
            width:parent.width
			
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
						filemenu.fileOperationClicked(index)
					}
					enabled: filemenu.buttonsenabled[index];
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
        id: locationMenu
        color: "#F4F6F7"

        width: 150;
        anchors.left: actionMenu.right
        height: parent.height

        border.width: 1
        border.color: "darkgray"

        states: [
            State {
                name: "recentfiles"
                PropertyChanges { target: recentFiles ; visible: true }
				PropertyChanges { target: currentFile ; visible: false }
                PropertyChanges { target: computer ; visible: false }
                PropertyChanges { target: osf ; visible: false }
                PropertyChanges { target: dataLibrary ; visible: false }
            },
			State {
                name: "currentfile"
                PropertyChanges { target: recentFiles ; visible: false }
				PropertyChanges { target: currentFile ; visible: true }
                PropertyChanges { target: computer ; visible: false }
                PropertyChanges { target: osf ; visible: false}
                PropertyChanges { target: dataLibrary ; visible: false }
            },
            State {
                name: "computer"
                PropertyChanges { target: recentFiles ; visible: false }
				PropertyChanges { target: currentFile ; visible: false }
                PropertyChanges { target: computer ; visible: true }
                PropertyChanges { target: osf ; visible: false }
                PropertyChanges { target: dataLibrary ; visible: false }

            },
            State {
                name: "osf"
                PropertyChanges { target: recentFiles ; visible: false }
				PropertyChanges { target: currentFile ; visible: false }
                PropertyChanges { target: computer ; visible: false }
                PropertyChanges { target: osf ; visible: true }
                PropertyChanges { target: dataLibrary ; visible: false }

            },
            State {
                name: "datalibrary"
                PropertyChanges { target: recentFiles ; visible: false }
				PropertyChanges { target: currentFile ; visible: false }
                PropertyChanges { target: computer ; visible: false }
                PropertyChanges { target: osf ; visible: false}
                PropertyChanges { target: dataLibrary ; visible: true }
            }		
        ]

        Column {
            id: fileLocation

            anchors.top: parent.top
            anchors.topMargin: 5
            anchors.horizontalCenter: parent.horizontalCenter
            spacing: 6
            width:parent.width

            Button {
                id: locationRecentFiles
                text: fileMenu.resourcesbuttons[0]

                width:parent.width-6
                height: resource_button_height
                anchors.leftMargin: 3
                anchors.left: parent.left

                enabled: true
				visible: filemenuproperties.recentfiles_button_visible
                onClicked: locationMenu.state = "recentfiles"
            }
			
			
			Button {
                id: locationCurrentFile
                text: fileMenu.resourcesbuttons[1]

                width:parent.width-6
                height: resource_button_height
                anchors.leftMargin: 3
                anchors.left: parent.left

                enabled: true
				visible: filemenuproperties.currentfile_button_visible
                onClicked: locationMenu.state = "currentfile"
            }


            Button {
                id: locationComputer
                text: fileMenu.resourcesbuttons[2]

                width:parent.width-6
                height: resource_button_height
                anchors.leftMargin: 3
                anchors.left: parent.left

                enabled: true
                onClicked: locationMenu.state = "computer"
            }

            Button {
                id: alocationOSF
                text: fileMenu.resourcesbuttons[3]

                width:parent.width-6
                height: resource_button_height
                anchors.leftMargin: 3
                anchors.left: parent.left

                enabled: true
				onClicked: {
					
					locationMenu.state= "osf"
					filemenu.resourceButtonClicked(3)
				}
            }

            Button {
                id: locationDataLibrary
                text: fileMenu.resourcesbuttons[4]

                width:parent.width-6
                height: resource_button_height
                anchors.leftMargin: 3
                anchors.left: parent.left

                enabled: true
				visible: filemenuproperties.datalibrary_button_visible
                onClicked: locationMenu.state= "datalibrary"
            }
        } //Column Filelocation
    }//Rectangle Location Menu

    Transition {
        NumberAnimation { properties: "x,y"; duration: 500 }
    }

    focus: true
    Keys.onSpacePressed: locationMenu.visible = !locationMenu.visible

    Rectangle{

        id: resourceScreen

        anchors.left: locationMenu.right
        width: parent.width - actionMenu.width - locationMenu.width
        height: parent.height

        border.width: 1
        border.color: "darkgray"

        BackstageRecentFiles{
            id: recentFiles

            anchors.fill: parent
            anchors.margins: 1

            visible: true
        }

        BackstageCurrentFile{
            id: currentFile

            anchors.fill: parent
            anchors.margins: 1

            visible: false
        }

        BackstageComputer{
            id: computer

            anchors.fill: parent
            anchors.margins: 1

            visible: false
        }

        BackstageOSF{
            id: osf

            anchors.fill: parent
            anchors.margins: 1

            visible: false
        }

        BackstageDataLibrary{

            id: dataLibrary

            anchors.fill: parent
            anchors.margins: 1

            visible: true
        }
    }  //Rectangle resourceScreen
}
