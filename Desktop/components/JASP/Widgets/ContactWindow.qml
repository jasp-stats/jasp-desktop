import QtQuick
import JASP.Widgets
import JASP.Controls
import QtQuick.Controls as QC

WavyWindow
{
	id:				contactWindow

	visible:				mainWindow.contactVisible
	onVisibleChanged:		mainWindow.contactVisible = visible
	onCloseModel:			{ mainWindow.contactVisible = false }

	title:					qsTr("Contact JASP")

	QC.TextArea
	{
		id:				contactText
		textFormat:		Text.RichText
		text:
qsTr("For <a href=\"%1\">feature requests</a> and <a href=\"%2\">bug reports</a>: please post an issue on our GitHub page, <a href=\"%3\">as explained here.</a>
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
		color:			jaspTheme.textEnabled
		leftPadding:	0
		topPadding:		0
		bottomPadding:	0
		wrapMode:		TextEdit.Wrap
		font.family:	jaspTheme.font
		font.pixelSize:	16 * jaspTheme.uiScale
		width:			parent.width

		selectByMouse:	true
		readOnly:		true

		onPressed:		(event)=>{ if (event.button === Qt.RightButton)	contextMenu.popup()  }

		QC.Menu
		{
			id:		contextMenu
			width:	120

			QC.Action { text: qsTr("Select All");		onTriggered: contactText.selectAll();	}
			QC.Action { text: qsTr("Copy Selection");	onTriggered: contactText.copy();		}
		}

		onLinkActivated:	(link)=>{ Qt.openUrlExternally(link) }

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

