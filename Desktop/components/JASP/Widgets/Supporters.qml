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
		spacing:					welcomeRoot.scaler * 20
		anchors.left:				parent.left
		anchors.right:				parent.right

		Text
		{
			text:					qsTr("JASP is supported by the following institutions:").replace(/, /g, ",&nbsp;").replace(/\n\n/g, "<br><br>")
			font.family:			jaspTheme.font
			font.weight:			Font.Normal
			font.pixelSize:         fontPixelSize * welcomeRoot.scaler
			color:					jaspTheme.white
			wrapMode:				TextEdit.Wrap
			renderType:				Text.QtRendering
			textFormat:				Text.StyledText
			horizontalAlignment:	Text.AlignHCenter
			Layout.fillWidth:		true
		}

		SwipeView
		{
			id:						swipeSupporters
			orientation:			Qt.Vertical
			Layout.fillWidth:		true
			clip:					true
			currentIndex:			Math.floor(Math.random() * count)
			wheelEnabled:			true

			Timer
			{
				interval:			5000
				repeat:				true
				running:			true
				onTriggered:		{ swipeSupporters.setCurrentIndex((swipeSupporters.currentIndex + 1) % swipeSupporters.count) }
			}

			Repeater
			{
				model:	mainWindow.coopThankYou

				Text
				{
					text:					String(modelData).replace(/&/g, "&amp;").replace(/, /g, ",&nbsp;").replace(/\n/g, "<br>")
					font.family:			jaspTheme.font
					font.weight:			Font.Bold
					font.pixelSize:         (4 + fontPixelSize) * welcomeRoot.scaler
					color:					jaspTheme.white
					wrapMode:				TextEdit.Wrap
					renderType:				Text.QtRendering
					textFormat:				Text.StyledText
					horizontalAlignment:	Text.AlignHCenter

					width:					swipeSupporters.width
				}
			}


		}

		Text
		{
			text:					qsTr("Suggest your institution joins the JASP Cooperative").replace(/, /g, ",&nbsp;").replace(/\n\n/g, "<br><br>")
			font.family:			jaspTheme.font
			font.weight:			Font.Normal
			font.pixelSize:         fontPixelSize * welcomeRoot.scaler
			font.underline:			true
			color:					jaspTheme.white
			wrapMode:				TextEdit.Wrap
			renderType:				Text.QtRendering
			textFormat:				Text.StyledText
			horizontalAlignment:	Text.AlignHCenter
			Layout.fillWidth:		true
		}
	}

	MouseArea
	{
		cursorShape:			Qt.PointingHandCursor
		acceptedButtons:		Qt.LeftButton
		anchors.fill:			parent
		onClicked:				Qt.openUrlExternally(mainWindow.coopHowToSupport)
		z:						-8
	}
}
