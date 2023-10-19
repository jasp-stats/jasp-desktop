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
	onCloseModel:			{ mainWindow.contactVisible = false }
	width:					800 * jaspTheme.uiScale
	height:					600 * jaspTheme.uiScale


	Text
	{
		id:				contactText
		textFormat:		Text.RichText
		text:
qsTr("<h3>Contact</h3>
For <a href=\"%1\">feature requests</a> and <a href=\"%2\">bug reports</a>: please post an issue on our GitHub page, <a href=\"%3\">as explained here.</a>
This will bring you in direct contact with the JASP software developers.

For statistical questions: please post an issue <a href=\"%4\">on the JASP Forum.</a>

For information on the JASP Cooperative: please read <a href=\"%5\">the information on the JASP website</a>

For suggesting we add your institution to the <a href=\"%6\">JASP World Map</a> please send an email to <a href=\"%7\">communications@jasp-stats.org</a>.

For individual donations: please visit <a href=\"%8\">the JASP website</a>.
")
.arg("https://github.com/jasp-stats/jasp-issues/issues/new?assignees=&labels=Feature+Request&projects=&template=feature-request.yml&title=%5BFeature+Request%5D%3A+")
.arg("https://github.com/jasp-stats/jasp-issues/issues/new?assignees=&labels=Bug&projects=&template=bug-report.yml&title=%5BBug%5D%3A+")
.arg("https://jasp-stats.org/2018/03/29/request-feature-report-bug-jasp/")
.arg("https://forum.cogsci.nl/index.php?p=/categories/jasp-bayesfactor")
.arg(mainWindow.coopUrl)
.arg("https://jasp-stats.org/world-map/")
.arg("mailto:communications@jasp-stats.org")
.arg("https://jasp-stats.org/donate/")
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
		
		onLinkActivated:	(link)=>{ Qt.openUrlExternally(link) }
		
		MouseArea
		{
			id:					mouseAreaContactText
			anchors.fill:		parent
			acceptedButtons:	Qt.LeftButton
			onClicked:			(event)=>{ if (event.button === Qt.LeftButton && myText.linkHovered) Qt.openUrlExternally(myText.hoveredLink)  }
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

