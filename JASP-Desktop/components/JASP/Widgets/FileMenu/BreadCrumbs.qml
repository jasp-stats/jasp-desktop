import QtQuick 2.0
import QtQuick.Layouts 1.3
import QtQuick.Controls 2.2
import JASP.Theme 1.0
import JASP.Widgets 1.0

ListView
{
	id : listView
			
	orientation: ListView.Horizontal

	signal crumbButtonClicked(int modelIndex)
	
	delegate:
		Item
		{
			id:		rect
			height: rectButton.height
			width:	rectArrow.width + rectButton.width
			
			Item
			{
				id :	rectArrow
				//color:	Theme.grayMuchLighter
				
				height:		rect.height
				width:		model.index > 0 ? height   : 0
				visible:	model.index > 0

				Image {
					id:					rightArrow
					anchors.centerIn: 	parent
					source:				"qrc:/icons/right-arrow.png"
					sourceSize.width:	parent.width * 2
					sourceSize.height:	parent.height * 2

					height:				parent.height - 4
					width:				parent.width - 4
				}
			}

			FilterButton
			{
				id:				rectButton
				anchors.left:	model.index > 0 ? rectArrow.right : rect.left
				text:			model.name
				enabled:		index < count - 1
				toolTip:		index < count - 1 ? "Back to " + model.name : "You are here"
				onClicked:		listView.crumbButtonClicked(model.index)
			}
		}
}
