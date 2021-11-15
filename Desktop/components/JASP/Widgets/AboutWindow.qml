import QtQuick 2.11
import QtQuick.Window 2.11
import QtQuick.Controls 2.5
import QtWebEngine 1.7
import JASP.Widgets 1.0
import JASP.Controls 1.0 as JC


Window
{
	id:				aboutWindow

	width:			850 * preferencesModel.uiScale
	height:			550 * preferencesModel.uiScale
    
	minimumWidth:	850 
	minimumHeight:	550 

	// maximumWidth:	850 
	maximumHeight:	550 
   
	property string labelcolor:			"#F99800"
	property string closebuttoncolor:	"#50B0E3"

	visible:				aboutModel.visible
	onVisibleChanged:		aboutModel.visible = visible
	title:					qsTr("About JASP")

	color:			jaspTheme.white

	Shortcut { onActivated: aboutModel.visible = false;	sequences: ["Ctrl+Q", "Ctrl+W", Qt.Key_Close]; }

	Connections
	{
		target:			mainWindow
		function onCloseWindows() { aboutModel.visible = false; }
	}


	Image
	{
		id:				topWave
		anchors.top:	parent.top
		anchors.left:	parent.left
		anchors.right:	parent.right
		height:			parent.height/4

		source:		jaspTheme.iconPath + "jasp-wave-down-blue-120.svg"
	}

	Item
	{
		id:				jaspLogo

		width:			parent.height / 3
		height:			parent.height / 5.5
		anchors
		{
			left:		parent.left
			leftMargin: 50 * preferencesModel.uiScale
			top:		aboutInfoBox.top
			topMargin:	25 * preferencesModel.uiScale
		}

		Image
		{
			anchors.fill:		parent
			fillMode:			Image.PreserveAspectFit
			source:				jaspTheme.iconPath + "jasp-logo-black.svg"
			sourceSize.width:	width  * 2
			sourceSize.height:	height * 2
			mipmap:				true
		}
	}

	Rectangle
	{
		id:							aboutInfoBox
		property int labelwidth:	Math.max(jaspVersionLabel.implicitWidth,
										  buildDateLabel.implicitWidth,
										  sourceLabel.implicitWidth,
										  downloadLabel.implicitWidth,
										  citationLabel.implicitWidth) + 10

		anchors
		{
			top:			topWave.bottom
			left:			jaspLogo.right
			right:			parent.right
			leftMargin:		25
			rightMargin:	15
		}
		height:			contentInfo.height
		color:			jaspTheme.white

		Column
		{
			id: contentInfo
			anchors.left:	parent.left
			anchors.top:	parent.top
			anchors.right:	parent.right
			spacing:		5 * preferencesModel.uiScale

			JC.Text { text: aboutModel.copyrightMessage; color: jaspTheme.textEnabled	}

			Row
			{
				JC.Label { text: "<b>" + qsTr("Version:") + "</b>";		color: labelcolor; width: aboutInfoBox.labelwidth; id: jaspVersionLabel }
				JC.Text	 { text: aboutModel.version;	color: jaspTheme.textEnabled											}
			}

			Row
			{
				JC.Label { text: "<b>" + qsTr("Built on:") + "</b>";		color: labelcolor; width: aboutInfoBox.labelwidth; id: buildDateLabel	}
				JC.Text	 { text: aboutModel.buildDate;	color: jaspTheme.textEnabled											}
			}

			Row
			{
				JC.Label { text: "<b>" + qsTr("Source:") + "</b>";		color: labelcolor; width: aboutInfoBox.labelwidth; id: sourceLabel		}
				JC.Text
				{
					text:		"<u>" + qsTr("Access the sources here") + "</u>"
					color:		jaspTheme.blue

					MouseArea
					{
						id:				mouseAreaSources
						anchors.fill:	parent
						onClicked:		Qt.openUrlExternally(aboutModel.commitUrl)
						cursorShape:	Qt.PointingHandCursor
					}
				}
			}

			Row
			{
				JC.Label { text: "<b>" + qsTr("Download:") + "</b>";		color:	labelcolor; width: aboutInfoBox.labelwidth; id:	downloadLabel	}
				JC.Text
				{
					text:			"<u>" + aboutModel.downloadUrl + "</u>"
					color:			jaspTheme.blue

					MouseArea
					{
						id:				mouseAreaDownload
						anchors.fill:	parent
						onClicked:		Qt.openUrlExternally(aboutModel.downloadUrl)
						cursorShape:	Qt.PointingHandCursor

					}
				}
			}

			Row
			{
				JC.Label
				{
					id:				citationLabel
					width:			aboutInfoBox.labelwidth
					text:			"<b>" + qsTr("Citation:") + "</b>"
					color:			labelcolor
				}

				Column
				{
					TextArea
					{
						id:				citationText
						textFormat:		Text.StyledText
						text:			aboutModel.citation
						color:			jaspTheme.textEnabled
						leftPadding:	0
						topPadding:		0
						bottomPadding:	0
						width:			aboutInfoBox.width - aboutInfoBox.labelwidth
						wrapMode:		TextEdit.WordWrap
						font:			jaspTheme.font

						selectByMouse:	true
						readOnly:		true

						onPressed:		if (event.button === Qt.RightButton)	contextMenu.popup()

						Menu
						{
							id:		contextMenu
							width:	120

							Action { text: qsTr("Select All");		onTriggered: citationText.selectAll();	}
							Action { text: qsTr("Copy Selection");	onTriggered: citationText.copy();		} //citationText.deselect(); is not really necessary right?
						}
					}

					JC.Text
					{
						text:			"<u>" + qsTr("BibTeX") + "</u>"
						color:			jaspTheme.blue

						MouseArea
						{
							id:				mouseAreaBibTex
							anchors.fill:	parent
							onClicked:		Qt.openUrlExternally(aboutModel.citationUrl)
							cursorShape:	Qt.PointingHandCursor
						}
					}
				}
			}

			JC.Text
			{
				id:				warrantyText
				text:			aboutModel.warranty
				textFormat:		Text.StyledText
				opacity:		0.5
				color:			jaspTheme.textEnabled
			}

			JC.Text
			{
				id:				openSourceText
				text:			"<u>" + qsTr("Open Source Components") + "</u>"
				color:			jaspTheme.blue

				MouseArea
				{
					id:				mouseAreaOpenSource
					anchors.fill:	parent
					onClicked:		Qt.openUrlExternally(aboutModel.openSourceUrl)
					cursorShape:	Qt.PointingHandCursor
				}
			}
		}
	}

	Rectangle
	{
		id:				closeButton
		x:				aboutWindow.width / 2 - parent.x -width / 2
		anchors.top:	aboutInfoBox.bottom
		anchors.topMargin: 10 * preferencesModel.uiScale
		color:			closebuttoncolor
		radius:			5 * preferencesModel.uiScale
		height:			25 * preferencesModel.uiScale
		width:			75 * preferencesModel.uiScale

		JC.Text
		{
			anchors.centerIn:		parent
			text:					qsTr("Close")
			color:					jaspTheme.white
			horizontalAlignment:	Text.AlignHCenter
			verticalAlignment:		Text.AlignVCenter
		}

		MouseArea
		{
			onClicked:				{aboutModel.visible = false; aboutWindow.close()}
			anchors.fill:			parent
			cursorShape:			Qt.PointingHandCursor
			focus:					true
			Keys.onEnterPressed:	{aboutModel.visible = false; aboutWindow.close()}
			Keys.onReturnPressed: (event)=>	{aboutModel.visible = false; aboutWindow.close()}
			Keys.onEscapePressed:	{aboutModel.visible = false; aboutWindow.close()}
		}
	}

	Image
	{
		id:					bottomWave
		z:					-1

		anchors.bottom:		parent.bottom
		anchors.left:		parent.left
		anchors.right:		parent.right
		height:				parent.height/4
		source:				jaspTheme.iconPath + "jasp-wave-up-green-120.svg"
	}
}

