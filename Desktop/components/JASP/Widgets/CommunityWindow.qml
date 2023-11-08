import QtQuick
import JASP.Widgets
import JASP.Controls
import QtQuick.Controls as QC

WavyWindow
{
	id:						communityWindow
	title:					qsTr("JASP Community")
	height:					700
	width:					1200

	visible:				mainWindow.communityVisible
	onVisibleChanged:		mainWindow.communityVisible = visible
	onCloseModel:			{ mainWindow.communityVisible = false }
	
	


	Text
	{
		id:				communityText
		textFormat:		Text.RichText
		text:
			qsTr("<h3>Community</h3>
The institutions of higher learning that participate in the JASP community jointly support the maintenance and further development of JASP, therefore providing an invaluable educational service to their own students and to those of other institutions worldwide.

If your institution is not yet part of the JASP community, you can <a href=\"%1\">suggest that they join</a>.

<i><b>Educator Institutions:</b></i>
%2
                 
<i><b>Sponsor Institutions:</b></i>
%3

<i><b>Supporter Institutions:</b></i>
%4")
.arg(mainWindow.coopHowToSupport)
.arg(mainWindow.coopEducators)
.arg(mainWindow.coopSponsors)
.arg(mainWindow.coopSupporters)
.replace(/&/g, "&amp;").replace(/, /g, ",&nbsp;").replace(/\n/g, "<br>")
        
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

		MouseArea
		{
			id:					mouseAreaContactText
			anchors.fill:		parent
			acceptedButtons:	Qt.LeftButton
			onClicked:			(event)=>{ if (event.button === Qt.LeftButton && myText.linkHovered) Qt.openUrlExternally(myText.hoveredLink)  }
			cursorShape:		myText.linkHovered ?  Qt.PointingHandCursor  : Qt.ArrowCursor
			
			property Item myText:	communityText
		}

		MouseArea
		{
			id:					cursorShower
			acceptedButtons:	Qt.NoButton
			hoverEnabled:		true
			anchors.fill:		parent

			cursorShape:		!containsMouse || communityText.linkAt(mouseX, mouseY) === "" ? Qt.ArrowCursor : Qt.PointingHandCursor
		}
	}

}

