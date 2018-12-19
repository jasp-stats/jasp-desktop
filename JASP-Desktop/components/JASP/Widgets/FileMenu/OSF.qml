import QtQuick 2.0
import QtQuick.Controls 2.2
import JASP.Theme 1.0
import JASP.Widgets 1.0

Item
{
	id:		rect
	
	property bool loggedin:			fileMenuModel.osf.loggedin
	property bool processing:		fileMenuModel.osf.processing
	property bool showfiledialog:	fileMenuModel.osf.showfiledialog
	
	
	Label
	{
		id:headLabel
		
		width:	implicitWidth
		height:	30
		anchors
		{
			top:		parent.top
			left:		parent.left
			leftMargin: 12
			topMargin:	12
			right:		parent.right
		}
		verticalAlignment: Text.AlignVCenter
		
		text: "Open Science Framework"
		font: Theme.fontLabel
		color: Theme.black
	}
	
	
	FilterButton {
		id: logoutButton
		
		visible: loggedin
				
		text: "Logout"
		width: 80
		height: 20
		anchors.right: parent.right
		anchors.top: parent.top
		anchors.rightMargin: 12
		anchors.topMargin: 12
		
		onClicked: {
			fileMenuModel.osf.logoutClicked();
		}
	}
	
	BreadCrumbs
	{
		id:		osfbreadcrumbs
		
		visible: loggedin
		
		model : fileMenuModel.osf.breadCrumbs
		
		width:				rect.width
		height:				loggedin ? 40 : 0
		anchors.top:		headLabel.bottom
		anchors.left:		parent.left
		anchors.right:		parent.right
		anchors.leftMargin:	12  //Position datalibrary breadcrumbs

		onCrumbButtonClicked: fileMenuModel.osf.breadCrumbs.indexChanged(modelIndex);
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
			
			width:	80
			height: 30
			anchors
			{
				top:		firstSeparator.bottom
				left:		parent.left
				leftMargin: 12
				topMargin:	6
				right:		parent.right
			}
			
			text :			"Filename"
			font.family:	"SansSerif"
			font.pixelSize: 14
			color:			Theme.black
			verticalAlignment: Text.AlignVCenter
		}
		
		Rectangle
		{
			
			id: saveFilenameInput
			
			anchors
			{
				left: saveFilenameLabel.right
				leftMargin: 6
				top: saveFilenameLabel.top
				right: parent.right
				rightMargin: 12
			}
			height: saveFilenameLabel.height
			clip: true
			
			color: Theme.white
			border.width: filenameText.activeFocus ? 5 : 1
			border.color: filenameText.activeFocus ? Theme.focusBorderColor : Theme.grayDarker
			
			TextInput {
				
				id: filenameText
				
				anchors.fill: parent
				anchors.leftMargin: 10
				selectByMouse: true
				
				text: fileMenuModel.osf.savefilename
				
				verticalAlignment: Text.AlignVCenter			
				font.pixelSize: 14
				
				onAccepted: {
					fileMenuModel.osf.saveFile(filenameText.text)
				}
			}		
		}
		
		FilterButton {
			id: newDirectoryButton
					
			text: "New Folder"
			width: 100
			height: 20
			anchors.right: saveFilenameButton.left
			anchors.top: saveFilenameInput.bottom
			anchors.rightMargin: 12
			anchors.topMargin: 12
			
			onClicked: {
				fileMenuModel.osf.newFolderClicked()
			}
		}
		
		FilterButton {
			id: saveFilenameButton

			text: "Save"
			width: 80
			height: 20
			anchors.right: parent.right
			anchors.top: newDirectoryButton.top
			anchors.rightMargin: 12
			
			onClicked: {
				fileMenuModel.osf.saveFile(filenameText.text)
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
	
	Item {
		
		visible: processing
		
		width: animation.width; 
		height: animation.height + 8
		
		anchors.horizontalCenter: osfList.horizontalCenter
		anchors.verticalCenter: osfList.verticalCenter
		
		
		AnimatedImage { id: animation; source: "qrc:/icons/loading.gif" }
		
	}
	
	FileList {
		id:			osfList
		visible:	loggedin && !processing
		cppModel:	fileMenuModel.osf.listModel
		
		anchors
		{
			top:			secondSeparator.bottom
			left:			parent.left
			right:			parent.right
			bottom:			thirdSeparator.top
			leftMargin:		12  //Position datalibrary items
			topMargin:		Theme.generalAnchorMargin
			bottomMargin:	Theme.generalAnchorMargin
			rightMargin:	Theme.generalAnchorMargin
		}
		
	}
	
	OSFLogin {
		id: osfLogin
		
		visible:				!loggedin && !processing
		height:					170
		
		anchors.top:			secondSeparator.bottom
		anchors.left:			parent.left
		anchors.right:			parent.right
		anchors.leftMargin:		12
		anchors.topMargin:		6
		anchors.rightMargin:	6
		
	}
	
	ToolSeparator
	{
		id:						thirdSeparator
		
		anchors.bottom:			linkOSF.top
		width:					rect.width
		orientation:			Qt.Horizontal
		anchors.bottomMargin:	5
	}
	
	Text {
		id:						linkOSF
		
		height:					30
		width:					implicitWidth
		anchors.left:			parent.left
		anchors.leftMargin:		12
		anchors.bottom:			parent.bottom
		textFormat:				Text.StyledText
		
		text:'<font color="blue"><u>About the OSF</u></font>'
		MouseArea {
			anchors.fill:	parent
			hoverEnabled:	true
			cursorShape:	containsMouse ? Qt.PointingHandCursor : Qt.ArrowCursor
			onClicked:		Qt.openUrlExternally("http://help.osf.io")
		}
	}
	
	Text {
		id:			linkRegister
		
		height:		30
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
