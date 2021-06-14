import QtQuick			2.12
import QtQuick.Window	2.12
import QtWebEngine		1.8
import JASP.Widgets		1.0
import JASP.Controls	1.0

Window
{
	id:					enginesWindow
	width:				400 * jaspTheme.uiScale
	height:				Screen.height
	minimumWidth:		200 * preferencesModel.uiScale
	minimumHeight:		minimumWidth
	title:				qsTr("Engines Overview")
	color:				jaspTheme.uiBackground
	visible:			true
	x:					0
	y:					0

	Shortcut { onActivated: enginesWindow.close();				sequences: ["Ctrl+W", Qt.Key_Close];							}


	Connections
	{
		target:			mainWindow
		function onCloseWindows() { enginesWindow.close(); }
	}


	ListView
	{
		anchors.fill:	parent
		model:			engineSync


		delegate:	Item
		{

			height:			150 * jaspTheme.uiScale
			width:			ListView.view.width

			Rectangle
			{
				color:					jaspTheme.uiBackground
				border.color:			jaspTheme.uiBorder
				border.width:			1
				radius:					jaspTheme.borderRadius * 2

				anchors.fill:			parent
				anchors.margins:		jaspTheme.generalAnchorMargin

				Text
				{
					text:				model.display + "\nState: " + model.engineState + "\nModule: " + model.module + (model.analysisStatus === "NA" ? "" : "\nAnalysis Status: " + model.analysisStatus)
					font.pixelSize:		20 * jaspTheme.uiScale
					font.bold:			true
					anchors
					{
						left:			parent.left
						leftMargin:		jaspTheme.generalAnchorMargin
						verticalCenter: parent.verticalCenter
					}
				}

				Rectangle
				{
					height: parent.height * 0.5
					width:	height
					radius: height

					border.color:		jaspTheme.black
					border.width:		6 * jaspTheme.uiScale
					color:				model.idle ? "green" : model.analysisStatus !== "NA" ? jaspTheme.jaspBlue : model.idleSoon ? "yellow" : "orange"

					anchors
					{
						right:			parent.right
						rightMargin:	jaspTheme.generalAnchorMargin
						verticalCenter:	parent.verticalCenter
					}
			}
		}

		JASPMouseAreaToolTipped
		{
			acceptedButtons:	Qt.MiddleButton

			toolTipText:		qsTr("Middle mouse button to kill an engine.")



			onClicked: (mouse) =>
			{
				messages.log("engine pressed " + model.engineState + " #" + model.channel);
				if(mouse.button == Qt.MiddleButton)
				{
					//messages.log("killing!");
					engineSync.killEngine(model.channel);
				}
			}
		}


	}
}
}
