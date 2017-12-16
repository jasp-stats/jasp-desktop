import QtQuick 2.0
import QtQuick.Layouts 1.3
import QtQuick.Controls 2.2

ListView
{
	anchors.left:parent.left
	anchors.right:parent.right
	anchors.leftMargin: 15

	model: dataLibraryBreadCrumbsModel

	orientation: ListView.Horizontal

	delegate: RowLayout
	{
		Rectangle
		{
			id:rect
			height: 40
			width: 100
			color: "lightblue"
			radius: 15
			border.color: "white"
			border.width: 2
			Button
			{
				id:crumbbutton
				text: model.name
				Layout.fillWidth: true
				anchors.margins: 5
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
