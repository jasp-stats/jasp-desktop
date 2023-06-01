import QtQuick 2.0
import QtQuick.Layouts 1.3
import QtQuick.Controls 2.2
import JASP.Theme 1.0

ListView
{
	anchors.left:parent.left
	anchors.right:parent.right
	
	model: dataLibraryBreadCrumbsModel
	
	orientation: ListView.Horizontal
	
	delegate: RowLayout
	{
		anchors.verticalCenter : parent.verticalCenter
		
		Rectangle
		{
			id:rect
			height: 20
			width: rectArrow.width + rectButton.width 
			
			Rectangle {
				id : rectArrow
				color: Theme.grayMuchLighter
				
				height: rect.height 
				width: model.index > 0 ? height   : 0
				anchors.verticalCenter: rectButton.verticalCenter
				visible: model.index > 0
				
				Image {
					anchors.fill: parent
					id: rightArrow
					source: "qrc:/icons/right-arrow.png"
				}
			}
			
			Rectangle
			{
				id: rectButton
				height: rect.height
				width: 100				
				border.color: Theme.gray
				border.width: 1
				
				anchors.left:  model.index > 0 ? rectArrow.right : rect.left
				
				Button
				{
					id:crumbbutton
					
					background: Rectangle {
						anchors.fill: parent
						gradient: Gradient {
							GradientStop { position: 0 ; color:  Theme.grayMuchLighter }
							GradientStop { position: 1 ; color:  Theme.white }
						}
					}
					
					MouseArea
					{
						id: crumbbuttonMouseArea
						hoverEnabled: true
						anchors.fill: parent
						cursorShape: Qt.PointingHandCursor 
						onClicked: {
							dataLibraryListModel.changePath(model.index);
						}					
					}
					
					text: model.name
					Layout.fillWidth: true
					height: rect.height
					width: rectButton.width
					anchors.margins: 1
					anchors.fill: parent
					ToolTip.delay: 500
					ToolTip.text:  index < count -1 ? "Back to " + model.name : model.name;
					ToolTip.visible: count > 1 ? hovered  : false
				}
			}			
		}	
	}
}
