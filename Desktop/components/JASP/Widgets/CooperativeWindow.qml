import QtQuick
import JASP.Widgets
import JASP.Controls
import QtQuick.Controls as QC

WavyWindow
{
	id:				cooperativeWindow

	visible:				mainWindow.cooperativeVisible
	onVisibleChanged:		mainWindow.cooperativeVisible = visible
	onCloseModel:			{ mainWindow.cooperativeVisible = false }

	title:					qsTr("JASP Cooperative")

	QC.TextArea
	{
		id:				cooperativeText
		textFormat:		Text.RichText
		text:
			qsTr("<h3>Cooperative</h3>
The institutions of higher learning that participate in the JASP Cooperative jointly support the maintenance and further development of JASP, therefore providing an invaluable educational service to their own students and to those of other institutions worldwide.

If your institution is not yet part of the cooperative, you can <a href=\"%2\">suggest that they join</a>.

<i>Educator Institutions:</i>
%1").arg(
	mainWindow.coopEducators
).arg(
	mainWindow.coopHowToSupport
).replace(/&/g, "&amp;").replace(/, /g, ",&nbsp;").replace(/\n/g, "<br>")
		color:			jaspTheme.textEnabled
		leftPadding:	0
		topPadding:		0
		bottomPadding:	0
		wrapMode:		TextEdit.Wrap
		font.family:	jaspTheme.font
		font.pixelSize:	16 * jaspTheme.uiScale
		width:			parent.width * 0.8

		selectByMouse:	true
		readOnly:		true

		onPressed:		(event)=>{ if (event.button === Qt.RightButton)	contextMenu.popup()  }

		QC.Menu
		{
			id:		contextMenu
			width:	120

			QC.Action { text: qsTr("Select All");		onTriggered: cooperativeText.selectAll();	}
			QC.Action { text: qsTr("Copy Selection");	onTriggered: cooperativeText.copy();		}
		}

		onLinkActivated:	(link)=>{ Qt.openUrlExternally(link) }

		MouseArea
		{
			id:					cursorShower
			acceptedButtons:	Qt.NoButton
			hoverEnabled:		true
			anchors.fill:		parent

			cursorShape:		!containsMouse || cooperativeText.linkAt(mouseX, mouseY) === "" ? Qt.ArrowCursor : Qt.PointingHandCursor
		}
	}

}

