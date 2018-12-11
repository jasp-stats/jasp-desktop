import QtQuick 2.0
import QtQuick.Controls 2.2
//import JASP.Theme 1.0


Rectangle
{
	id:rect
	
	objectName: "rect"
	color: "#ececec"
	
	property bool loggedin : backstageosf.loggedin
	property bool processing : backstageosf.processing
	property bool showfiledialog : backstageosf.showfiledialog
	
	
	Label
	{
		id:headLabel
		
		width:implicitWidth
		height:30
		anchors.top: parent.top
		anchors.left: parent.left
		anchors.leftMargin: 12
		anchors.topMargin: 12
		verticalAlignment: Text.AlignVCenter
		
		text: "Open Science Framework"
		font.family: "SansSerif"
		font.pixelSize: 18
		color: "black"
	}
	
	
	Button {
		id: logoutButton
		
		visible: loggedin
		
		background: Rectangle {
			anchors.fill: parent
			gradient: Gradient {
				GradientStop { position: 0 ; color:  "#e5e5e5" }
				GradientStop { position: 1 ; color:  "white" }
			}
			border.color: "gray"
			border.width: 1
		}
		
		text: "Logout"
		width: 80
		height: 20
		anchors.right: parent.right
		anchors.top: parent.top
		anchors.rightMargin: 12
		anchors.topMargin: 12
		
		onClicked: {
			backstageosf.logoutClicked();		
		}
	}
	
	BreadCrumbs
	{
		id:osfbreadcrumbs
		
		visible: loggedin
		
		model : osfBreadCrumbsListModel
		
		width: rect.width
		height: loggedin ? 40 : 0
		anchors.top: headLabel.bottom
		anchors.left: parent.left
		anchors.right: parent.right
		anchors.leftMargin: 12  //Position datalibrary breadcrumbs
	}
	
	
		
	Item  /////////////////////////// File dialog to save in OSF ////////////////////////////////////
	{
		
		id: fileExportDialog
		
		width: rect.width
		visible: showfiledialog
		anchors.top: osfbreadcrumbs.bottom
		anchors.topMargin: 6
		height: visible ? 90 : 0
		
		
		ToolSeparator
		{
			id: firstSeparator
			anchors.top: fileExportDialog.top
			width: rect.width
			orientation: Qt.Horizontal
		}
		
		Label {
			id : saveFilenameLabel
			
			width: 80
			height: 30
			anchors.top: firstSeparator.bottom
			anchors.left: parent.left
			anchors.leftMargin: 12
			anchors.topMargin: 6
			
			text : "Filename"
			font.family: "SansSerif"
			font.pixelSize: 14
			color: "black"
			verticalAlignment: Text.AlignVCenter
		}
		
		Rectangle{
			
			id: saveFilenameInput
			
			anchors.left: saveFilenameLabel.right
			anchors.leftMargin: 6		
			anchors.top: saveFilenameLabel.top			
			anchors.right: parent.right
			anchors.rightMargin: 12
			height: saveFilenameLabel.height
			clip: true
			
			color: "white"			
			border.width: filenameText.activeFocus ? 5 : 1
			border.color: filenameText.activeFocus ? Theme.focusBorderColor : "darkgray"
			
			TextInput {
				
				id: filenameText
				
				anchors.fill: parent
				anchors.leftMargin: 10
				selectByMouse: true
				
				text: backstageosf.savefilename
				
				verticalAlignment: Text.AlignVCenter			
				font.pixelSize: 14
				
				onAccepted: {
					backstageosf.saveFile(filenameText.text)
				}
			}		
		}
		
		Button {
			id: newDirectoryButton
			
			background: Rectangle {
				anchors.fill: parent
				gradient: Gradient {
					GradientStop { position: 0 ; color:  "#e5e5e5" }
					GradientStop { position: 1 ; color:  "white" }
				}
				border.color: "gray"
				border.width: 1
			}
			
			text: "New Folder"
			width: 100
			height: 20
			anchors.right: saveFilenameButton.left
			anchors.top: saveFilenameInput.bottom
			anchors.rightMargin: 12
			anchors.topMargin: 12
			
			onClicked: {
				backstageosf.newFolderClicked()
			}
		}
		
		Button {
			id: saveFilenameButton
			
			background: Rectangle {
				anchors.fill: parent
				gradient: Gradient {
					GradientStop { position: 0 ; color:  "#e5e5e5" }
					GradientStop { position: 1 ; color:  "white" }
				}
				border.color: "gray"
				border.width: 1
			}
			
			text: "Save"
			width: 80
			height: 20
			anchors.right: parent.right
			anchors.top: newDirectoryButton.top
			anchors.rightMargin: 12
			
			onClicked: {
				backstageosf.saveFile(filenameText.text)	
			}
		}					
	}
	//////////////////////////////////////////////////////////////////////////////////////
	
	
	
	
	ToolSeparator
	{
		id: secondSeparator
		anchors.top: fileExportDialog.bottom
		width: rect.width
		orientation: Qt.Horizontal
	}
	
	Rectangle {
		
		visible: processing
		
		width: animation.width; 
		height: animation.height + 8
		
		color: "#ececec"
		
		anchors.horizontalCenter: osfList.horizontalCenter
		anchors.verticalCenter: osfList.verticalCenter
		
		
		AnimatedImage { id: animation; source: "qrc:/icons/loading.gif" }
		
	}
	
	OSFList {
		id: osfList
		
		visible: loggedin && !processing
		
		anchors.top: secondSeparator.bottom
		anchors.bottom: thirdSeparator.top
		anchors.left: parent.left
		anchors.right: parent.right
		anchors.leftMargin: 12  //Position datalibrary items
		anchors.topMargin: 6 
		anchors.bottomMargin: 6
		
	}
	
	OSFLogin {
		id: osfLogin
		
		visible: !loggedin && !processing		
		height: 170
		
		anchors.top: secondSeparator.bottom
		anchors.left: parent.left
		anchors.right: parent.right
		anchors.leftMargin: 12
		anchors.topMargin: 6
		anchors.rightMargin: 6
		
	}
	
	ToolSeparator
	{
		id: thirdSeparator
		
		anchors.bottom: linkOSF.top
		width: rect.width
		orientation: Qt.Horizontal
		anchors.bottomMargin: 5
	}
	
	Text {
		id: linkOSF
		
		height: 30
		width: implicitWidth
		anchors.left: parent.left
		anchors.leftMargin: 12
		anchors.bottom: parent.bottom
		textFormat: Text.StyledText
		
		text:'<font color="blue"><u>About the OSF</u></font>'
		MouseArea {
			anchors.fill: parent
			hoverEnabled: true
			cursorShape: containsMouse ? Qt.PointingHandCursor : Qt.ArrowCursor
			onClicked: Qt.openUrlExternally("http://help.osf.io")
		}
	}
	
	Text {
		id: linkRegister
		
		height: 30
		width: implicitWidth
		anchors.left: linkOSF.right 
		anchors.leftMargin: 12
		anchors.bottom: parent.bottom
		textFormat: Text.StyledText
		
		text:'<font color="blue"><u>Register</u></font>'
		MouseArea {
			anchors.fill: parent
			hoverEnabled: true
			cursorShape: containsMouse ? Qt.PointingHandCursor : Qt.ArrowCursor
			onClicked: Qt.openUrlExternally("https://osf.io")
		}
	}
}
