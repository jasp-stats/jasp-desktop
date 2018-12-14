import QtQuick 2.0
import QtQuick.Controls 2.2
import JASP.Controls 1.0
import JASP.Theme 1.0

ListView  {
	
	id : listView
	
	property bool firsttimeclicked: false
	
	maximumFlickVelocity: 700
	
	clip: true
			
	spacing : 10
	
	model: fileMenuModel.computer.listModel
	
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
			
			border.color: Theme.grayDarker
			color: fileEntryMouseArea.containsMouse || (ListView.isCurrentItem && firsttimeclicked ) ?  "#dcdadb" : Theme.grayMuchLighter
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
				id:textFolder
				
				width: parent.width
				height: parent.height
				anchors.left: fileImage.right
				anchors.leftMargin: 10
				anchors.bottom: parent.bottom
				horizontalAlignment: Text.AlignLeft
				verticalAlignment: Text.AlignVCenter
				
				text: model.name
				font:	Theme.font
			}
			
			MouseArea {
				id: fileEntryMouseArea
				anchors.fill: parent
				hoverEnabled: true
				cursorShape: containsMouse ? Qt.PointingHandCursor : Qt.ArrowCursor
				
				onClicked: {
					listView.currentIndex = index
					firsttimeclicked = true	
					backstagecomputer.browsePath(model.path)
				}
							
				ToolTip {
					id: fileToolTip
					delay: 500
					text: "Click to open folder and choose file."
					visible: fileEntryMouseArea.containsMouse
				}					
			}
		}				
	}
}
