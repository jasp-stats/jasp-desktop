import QtQuick 2.0
import QtQuick.Controls 2.2
import QtQuick.Layouts 1.3
import QtQuick.Controls.Styles 1.0

ListView 
{
	id : listview
	
	width: 800
	height: 800
	
	clip: true
	
	spacing : 10
	
	model : dataLibraryListModel
	
	delegate: Rectangle 
	{
		id :rectDelegate
		width:parent.width
		height:model.description == "" ? 40 : 80;
		color: "#ececec"
		//color:"lightblue"
		border.width : 0
		border.color : "lightblue"
		
		Text {
			id:textDescription
			height: 0.5 * parent.height
			visible: model.description == "" ? false : true
			text: model.description
			anchors.horizontalCenter: parent.horizontalCenter
			anchors.top: parent.top	
			anchors.topMargin: 0.15 * height
		}
			
		Button	
		{
			id: jaspfileButton
			
			height: model.description == "" ? parent.height : 0.5 * parent.height
			
			anchors {
				left: parent.left
				right: model.associated_datafile !== "" ? parent.horizontalCenter : parent.right
				bottom: parent.bottom
				margins:2
			}
			
			text: model.name
			
			Image {
				id : fileimage
				height: 0.95 * parent.height
				fillMode: Image.PreserveAspectFit
				source: model.iconsource
				anchors.left: parent.left
				anchors.top: parent.top
			}
			
			contentItem: Text {
				text: model.name
				anchors.left : fileimage.right
				anchors.leftMargin: 10
				horizontalAlignment: Text.AlignLeft
				verticalAlignment: Text.AlignVCenter
				elide: Text.ElideRight
			}
			
			onDoubleClicked: {
				if (model.type === 3) //Folder type
					dataLibraryListModel.changePath(model.name);
				else
					dataLibraryListModel.openFile(model.path)				
			}
		}
		
		Button
		{
			id: datefileButton
			
			height: 0.5 * parent.height
			visible: model.associated_datafile !== "" 
			
			anchors {
				left: parent.horizontalCenter
				right : parent.right				
				bottom: parent.bottom
				margins:2
			}
			
			text: model.associated_datafile
			
			Image {
				id : datafileimage
				height: 0.95 * parent.height
				fillMode: Image.PreserveAspectFit
				source: model.dataiconsource
				anchors.left: parent.left
				anchors.top: parent.top
			}
			
			
			onDoubleClicked: {
				dataLibraryListModel.openFile(model.dirpath + model.associated_datafile)
			}				
		}						
	}
}



