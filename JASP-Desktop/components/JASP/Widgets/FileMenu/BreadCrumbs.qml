import QtQuick 2.0
import QtQuick.Layouts 1.3
import QtQuick.Controls 2.2
import JASP.Theme 1.0
import JASP.Widgets 1.0
import JASP.Controls 1.0

ListView
{
	id : listView
			
	orientation: ListView.Horizontal
	property alias scrollBarVisible: scrollBar.visible
	property alias scrollBarHeight: scrollBar.height

	clip: true

	JASPScrollBar
	{
		id:				scrollBar
		flickable:		listView
		manualAnchor:	true
		vertical:		false
		height:			10 * preferencesModel.uiScale

		anchors
		{
			left:			parent.left
			right:			parent.right
			bottom:			parent.bottom
		}
	}

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

				height:		rect.height
				width:		index > 0 ? height   : 0
				visible:	index > 0

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

			RectangularButton
			{
				id:				rectButton
				anchors.left:	index > 0 ? rectArrow.right : rect.left
				text:			name
				enabled:		index < count - 1
				toolTip:		index < count - 1 ? "Back to " + name : "You are here"
				onClicked:		listView.crumbButtonClicked(index)
			}
		}
}
