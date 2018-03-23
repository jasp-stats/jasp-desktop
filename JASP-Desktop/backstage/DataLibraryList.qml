import QtQuick 2.0
import QtQuick.Controls 2.2
import QtQuick.Layouts 1.3

ListView 
{
	id : listview
	
	width: 800
	height: 500
	
	clip: true
		
	model : dataLibraryListModel
	
	delegate: RowLayout 
	{
		width:parent.width
		
		Button	
		{
			height:475
			anchors.left: parent.left
			anchors.right: model.associated_datafile !== "" ? parent.horizontalCenter : parent.right
			anchors.top: parent.top
			anchors.bottom: parent.bottom
			anchors.margins: 2
			text: model.name
			clip:true
			
			Image {
				id : fileimage
				height: 0.95 * parent.height
				fillMode: Image.PreserveAspectFit
				source: model.iconsource
				anchors.left: parent.left
				anchors.top: parent.top
			}
			
			onClicked: { }
			
			onDoubleClicked: {
				if (model.type === 3) //Folder type
					dataLibraryListModel.changePath(model.name);
				else
					dataLibraryListModel.openFile(model.path)				
			}
		}
		
		Button
		{
			anchors.right: parent.right
			anchors.left: parent.horizontalCenter
			anchors.top: parent.top
			anchors.bottom: parent.bottom
			anchors.margins: 2    
			visible: model.associated_datafile !== "" 
			text: model.associated_datafile
			onDoubleClicked: {
				dataLibraryListModel.openFile(model.dirpath + model.associated_datafile)
			}				
		}						
	}			
}


