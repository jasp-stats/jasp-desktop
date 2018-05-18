import QtQuick 2.0
import QtQuick.Layouts 1.3
import QtQuick.Controls 2.2

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
			width: 100
			border.color: "black"
			border.width: 1
			Button
			{
				id:crumbbutton
				
				background: Rectangle {
							anchors.fill: parent
							gradient: Gradient {
								GradientStop { position: 0 ; color:  "lightgray" }
								GradientStop { position: 1 ; color:  "white" }
							}
				}
				
				
				text: model.name
				Layout.fillWidth: true
				anchors.margins: 1
				anchors.fill: parent
				ToolTip.text:  index < count -1 ? "Back to " + model.name : model.name;
				ToolTip.visible: count > 1 ? hovered  : false
				onClicked: {
					dataLibraryListModel.changePath(model.name);
				}
			}
		}

		Text {
			id: arrow
			height: 10
			width: 20
			anchors.verticalCenter: rect.verticalCenter
			anchors.right: rect.left
			text: model.index > 0 ? "-->" : "";
		}
	}
}
