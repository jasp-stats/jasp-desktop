import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import JASP.Widgets


Item
{
	height:							supportColumn.height
	property int fontPixelSize:		14

	ColumnLayout
	{
		id:							supportColumn
		spacing:					welcomeRoot.scaler * 10
		anchors.left:				parent.left
		anchors.right:				parent.right

		Text
		{
			text:					qsTr("JASP is supported by the following institutions:")
			font.family:			jaspTheme.font.family
			font.weight:			Font.Normal
			font.pixelSize:         fontPixelSize * welcomeRoot.scaler
			color:					jaspTheme.white
			wrapMode:				TextEdit.Wrap
			renderType:				Text.QtRendering
			textFormat:				Text.StyledText
			horizontalAlignment:	Text.AlignHCenter
			Layout.fillWidth:		true
		}

		ListView
		{
			id:						swipeSupporters
			orientation:			Qt.Vertical
			Layout.fillWidth:		true
			clip:					true
			currentIndex:			Math.floor(Math.random() * count)
			//wheelEnabled:			true
            height:                 65 * welcomeRoot.scaler
            model:                  mainWindow.coopThankYou
            snapMode:               ListView.SnapOneItem
            onCurrentIndexChanged:  gottaMoveItMoveIt.restart()

            Timer
			{
                id:                 gottaMoveItMoveIt // https://www.youtube.com/watch?v=vuo8kD5zF5I
				interval:			5000
				repeat:				true
				running:			true
				onTriggered:		{ swipeSupporters.currentIndex = ((swipeSupporters.currentIndex + 1) % swipeSupporters.count) }
			}



            delegate:   Text
            {
                text:					String(modelData).replace(/&/g, "&amp;").replace(/\n/g, "<br>")
                font.family:			jaspTheme.font.family
                font.weight:			Font.Bold
                font.pixelSize:         (4 + fontPixelSize) * welcomeRoot.scaler
                color:					jaspTheme.white
                wrapMode:				TextEdit.Wrap
                renderType:				Text.QtRendering
                textFormat:				Text.StyledText
                horizontalAlignment:	Text.AlignHCenter
                verticalAlignment:      Text.AlignVCenter
                
                height:                 swipeSupporters.height
                width:					swipeSupporters.width
            }
			


		}

		Text
		{
			text:					qsTr("Suggest your institution joins the JASP Community").replace(/, /g, ",&nbsp;").replace(/\n\n/g, "<br><br>")
			font.family:			jaspTheme.font.family
			font.weight:			Font.Normal
			font.pixelSize:         fontPixelSize * welcomeRoot.scaler
			font.underline:			true
			color:					jaspTheme.white
			wrapMode:				TextEdit.Wrap
			renderType:				Text.QtRendering
			textFormat:				Text.StyledText
			horizontalAlignment:	Text.AlignHCenter
			Layout.fillWidth:		true


			MouseArea
			{
				cursorShape:			Qt.PointingHandCursor
				acceptedButtons:		Qt.LeftButton
				anchors.fill:			parent
				onClicked:				Qt.openUrlExternally(mainWindow.coopHowToSupport)
				z:						-8
			}
		}
	}
}
