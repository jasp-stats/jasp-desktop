import QtQuick 2.0
import QtQuick.Layouts 1.3
import QtQuick.Controls 2.2
import JASP.Theme 1.0

ListView
{
	id : listView
			
	orientation: ListView.Horizontal

	signal crumbButtonClicked(int modelIndex)
	
	delegate:
		Item
		{
			id:		rect
			height: 20
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
					anchors.fill:		parent
					anchors.margins:	2
					source:				"qrc:/icons/right-arrow.png"
					sourceSize.width:	parent.width * 2
					sourceSize.height:	parent.height * 2
				}
			}
			
			Rectangle
			{
				id:				rectButton
				height:			crumbbutton.contentHeight + (crumbbutton.anchors.margins * 2)
				width:			crumbbutton.contentWidth  + (crumbbutton.anchors.margins * 2)
				border.color:	Theme.gray
				border.width:	1
				
				anchors.left:	model.index > 0 ? rectArrow.right : rect.left
				
				color:			Theme.brokenWhite

				Text
				{
					id:	crumbbutton

					MouseArea
					{
						id:				crumbbuttonMouseArea
						hoverEnabled:	true
						anchors.fill:	parent
						cursorShape:	Qt.PointingHandCursor
						onClicked:		listView.crumbButtonClicked(model.index)
					}
					
					font:				Theme.font
					color:				Theme.black
					text:				model.name
					//height:				rect.height
					//width:				rectButton.width
					anchors.margins:	4
					anchors.fill:		parent
					ToolTip.delay:		500
					ToolTip.text:		index < count -1 ? "Back to " + model.name : model.name;
					ToolTip.visible:	count > 1 ? hovered  : false
				}
			}			
		}
}
