import QtQuick 2.0
import QtQuick.Controls 2.2
import JASP.Controls 1.0

ListView  {
	
	id : listView
	
	property bool firsttimeclicked: false
	
	maximumFlickVelocity: 700
	
	clip: true
	
	spacing : 10
	
	model: fileMenuModel.recentFiles.listModel
	
	delegate: modelDelegate
	
	JASPScrollBar {
		id: rightscrollbar
		flickable: parent
	}
	
	Component {
		
		id : modelDelegate
		
		Rectangle {
			
			id : rectFileEntry
			
			width: listView.width
			height: 40
			
			border.color: "darkgray"
			color: fileEntryMouseArea.containsMouse || (ListView.isCurrentItem && firsttimeclicked ) ?  "#dcdadb" : "#ececec"
			border.width: fileEntryMouseArea.containsMouse || ( ListView.isCurrentItem && firsttimeclicked ) ? 1 : 0
			
			Image {
				id : fileImage
				
				height: 0.95 * parent.height
				width: height
				anchors.left: parent.left
				anchors.top:rectFileEntry.top
				anchors.leftMargin: 10
				
				fillMode: Image.PreserveAspectFit
				source: model.iconsource				
			}
			
			Text {
				id:textFileName
				
				width: parent.width
				height: parent.height/2
				anchors.left: fileImage.right
				anchors.leftMargin: 10
				anchors.top: parent.top
				horizontalAlignment: Text.AlignLeft
				verticalAlignment: Text.AlignVCenter
				
				text: model.name
				font.family: "SansSerif"
				font.pixelSize: 12
			}
			
			Text {
				id:textFolder
				
				width: parent.width
				height: parent.height/2
				anchors.left: fileImage.right
				anchors.leftMargin: 10
				anchors.bottom: parent.bottom
				horizontalAlignment: Text.AlignLeft
				verticalAlignment: Text.AlignVCenter
				
				text: model.folder
				font.family: "SansSerif"
				font.pixelSize: 10				
			}
			
			MouseArea {
				id: fileEntryMouseArea
				anchors.fill: parent
				hoverEnabled: true
				cursorShape: containsMouse ? Qt.PointingHandCursor : Qt.ArrowCursor
				
				onClicked: {
					listView.currentIndex = index
					firsttimeclicked = true					
				}
				
				onDoubleClicked: {
					firsttimeclicked = false
					if (model.type !== 3) //Other then folder type
						recentFilesListModel.openFile(model.path)
				}
				
				ToolTip {
					id: fileToolTip
					delay: 500
					text: "Double click to open file"
					visible: fileEntryMouseArea.containsMouse
				}				
			}			
		}
	}	
}
