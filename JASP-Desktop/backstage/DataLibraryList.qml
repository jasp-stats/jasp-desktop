import QtQuick 2.0
import QtQuick.Controls 2.2
import QtQuick.Layouts 1.3
import QtQuick.Controls.Styles 1.0
import QtQuick.Window 2.3

ListView 
{
	id : listview
	
	clip: true
	
	ScrollBar.vertical: ScrollBar {}
	spacing : 10
	
	model : dataLibraryListModel
	
	delegate: libraryDelegate 
	
	function toolTipText(type, associated_datafile, mousearea)
	{
		//model type: JASP = 0, CSV = 1, SPSS = 2, Folder = 3, Other = 4, NoOfTypes = 5
		
		if (type === 3)  
			return "Press to navigate to folder"
		
		if ( (associated_datafile === "" && type === 0) || (associated_datafile !== "" && mousearea === "commonMouseArea") )
			return "Double click to open JASP file"	
		
		var a = ""
		if (associated_datafile !== "") a =" associated"
		return "Double click to open" + a + " data file"				
		
	}
	
	Component {
		id: libraryDelegate
		
		Rectangle {
			
			id: rectTitleAndDescripton
			
			width: listview.width
			height: rectTitle.height + rectDescription.height + 3
			border.width: 1
			border.color: "darkgray"
			
			color: "#ececec"  
			
			Rectangle {
				
				id: rectTitle
				
				height: 40 
				width: parent.width 
				anchors.left: parent.left
				anchors.right: parent.right
				anchors.top: parent.top
				anchors.margins: 1
				
				color: commonMouseArea.containsMouse ?  "darkgray" : "#dcdadb"
				
				Image {
					id : firstFileOrFolderImage
					
					height: 0.95 * parent.height
					width: height
					anchors.left: model.type===3 ? rectTitle.left : undefined
					anchors.right: model.type!==3 ? associatedDatafileImage.left : undefined 
					anchors.top:rectTitle.top
					anchors.leftMargin: 10
					
					fillMode: Image.PreserveAspectFit
					source: model.iconsource
					MouseArea {
						id: firstFileOrFolderMouseArea
						z:-2
						anchors.fill: parent
						hoverEnabled: true
						cursorShape: containsMouse ? Qt.PointingHandCursor : Qt.ArrowCursor
						
						onClicked: {
							if (model.type === 3) //Folder type
								dataLibraryListModel.changePath(model.name, model.path);
						}
						
						onDoubleClicked: {
							if (model.type !== 3) //Other then folder type
								dataLibraryListModel.openFile(model.path)				
						}
						
						
					}
					
					ToolTip {
						id: firstFileOrFolderTooltip
						delay: 500
						text: commonToolTip.text
						visible: firstFileOrFolderMouseArea.containsMouse
					}					
				}
				
				Image {
					id : associatedDatafileImage
					
					height: 0.95 * parent.height
					width: model.associated_datafile === "" ? 0 : height
					anchors.right : parent.right
					anchors.top:rectTitle.top
					anchors.rightMargin: 10
					
					fillMode: Image.PreserveAspectFit
					source: model.dataiconsource
					visible : model.associated_datafile !== ""
					
					MouseArea {
						z:-2
						id: datafileMouseArea
						anchors.fill: parent
						hoverEnabled: true
						
						onDoubleClicked: {
							dataLibraryListModel.openFile(model.dirpath + model.associated_datafile)				
						}
						cursorShape: containsMouse ? Qt.PointingHandCursor : Qt.ArrowCursor
					}
					
					ToolTip {
						id: datafileToolTip
						delay: 500
						text: toolTipText(model.type, model.associated_datafile, "datafileMouseArea")
						visible: datafileMouseArea.containsMouse
					}					
				}
				
				Text {
					id:textTitle
					
					height: parent.height
					anchors.top:parent.top
					anchors.left: model.type === 3 ? firstFileOrFolderImage.right : parent.left
					anchors.right:parent.right					
					anchors.leftMargin: 10
					
					text: model.name  //i.e. title
					horizontalAlignment: Text.AlignLeft
					verticalAlignment: Text.AlignVCenter					
				}
				
				MouseArea {
					z:-1
					id: commonMouseArea
					anchors.fill: parent
					hoverEnabled: true
					
					onClicked: {
						if (model.type === 3) //Folder type
							dataLibraryListModel.changePath(model.name, model.path);
					}
					
					onDoubleClicked: {
						if (model.type !== 3) //Other then folder type
							dataLibraryListModel.openFile(model.path)				
					}	
					
					cursorShape: containsMouse ? Qt.PointingHandCursor : Qt.ArrowCursor
					
				}
				
				ToolTip {
					id: commonToolTip
					delay: 500
					text: toolTipText(model.type, model.associated_datafile, "commonMouseArea")
					visible: commonMouseArea.containsMouse
				}
				
			}
			
			Rectangle {
				
				id: rectDescription	
				
				height : visible ? Math.max(40,textDescription.contentHeight) + 30 : 0
				anchors.left : parent.left
				anchors.right : parent.right
				anchors.top: rectTitle.bottom
				anchors.margins: 1
				
				color: "#ececec"
				visible: model.description !== ""
				
				Text {
					id:textDescription
					
					width: parent.width 
					height: parent.height 
					anchors.left: parent.left
					anchors.right: parent.right
					anchors.top: parent.top
					
					anchors.leftMargin: 10					
					anchors.rightMargin: 10
					anchors.topMargin: 10
					
					horizontalAlignment: Text.AlignJustify
					wrapMode: Text.WordWrap
					font.family: "SansSerif"
					font.pixelSize: 12
					textFormat: Text.StyledText				
					text: model.description								
				}				
			}						
		}		
	}
}




