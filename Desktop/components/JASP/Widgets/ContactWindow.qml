import QtQuick
import JASP.Widgets
import JASP.Controls
import QtQuick.Controls as QC

WavyWindow
{
	id:						contactWindow
	title:					qsTr("Contact JASP")
	visible:				mainWindow.contactVisible
	onVisibleChanged:		mainWindow.contactVisible = visible
	width:					800 * jaspTheme.uiScale
	height:					600 * jaspTheme.uiScale
	onCloseModel:
	{
		mainWindow.contactVisible = false
		contactWindow.close()
	}


	Text
	{
		id:				contactText
		textFormat:		Text.RichText
		text:			mainWindow.contactText

		color:					jaspTheme.textEnabled
		linkColor:				jaspTheme.jaspBlue
		leftPadding:			0
		topPadding:				0
		bottomPadding:			0
		wrapMode:				TextEdit.Wrap
		font.family:			jaspTheme.font
		font.pixelSize:			16 * jaspTheme.uiScale
		width:					parent.width
		horizontalAlignment:	Qt.AlignHCenter

		//selectByMouse:	true
		//readOnly:		true
		
		onLinkActivated:	(link)=>{ messages.log("onLinkActivated got link: " + link); Qt.openUrlExternally(link) }
		
		MouseArea
		{
			id:					mouseAreaContactText
			anchors.fill:		parent
			acceptedButtons:	Qt.LeftButton
			onClicked:			(event)=>{ if (event.button === Qt.LeftButton && myText.linkHovered)
									{
										messages.log("mouseAreaContactText got hoveredLink: " + myText.hoveredLink)
										Qt.openUrlExternally(myText.hoveredLink)  
									} }
									
			cursorShape:		myText.linkHovered ?  Qt.PointingHandCursor  : Qt.ArrowCursor
			
			property Item myText:	contactText
		}


		MouseArea
		{
			id:					cursorShower
			acceptedButtons:	Qt.NoButton
			hoverEnabled:		true
			anchors.fill:		parent

			cursorShape:		!containsMouse || contactText.linkAt(mouseX, mouseY) === "" ? Qt.ArrowCursor : Qt.PointingHandCursor
		}
	}

}

