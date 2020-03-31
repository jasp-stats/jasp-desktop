import QtQuick 2.11
import QtQuick.Window 2.11
import QtQuick.Controls 2.5
import QtQuick.Controls.Styles 1.4
import QtWebEngine 1.7
import JASP.Widgets 1.0


Window
{
	id:				aboutWindow

	width:			850
	height:			500

	maximumHeight:	height
	maximumWidth:	width
	minimumHeight:	height
	minimumWidth:	width

	property string labelcolor:			"#F99800"
	property string closebuttoncolor:	"#50B0E3"

	visible:				aboutModel.visible
	onVisibleChanged:		aboutModel.visible = visible
	title:					qsTr("About JASP")

	color:			jaspTheme.white

	Shortcut { onActivated: aboutModel.visible = false;	sequences: ["Ctrl+Q", "Ctrl+W", Qt.Key_Close]; }


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
			leftMargin: 50
			top:		aboutInfoBox.top
			topMargin:	25
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
			bottom:			bottomWave.top
			leftMargin:		25
			rightMargin:	25
		}

		border.color:	jaspTheme.gray
		border.width:	0
		color:			jaspTheme.white

		Text
		{
			id:				copyrightMessage
			anchors.left:	parent.left
			anchors.top:	parent.top
			anchors.right:	parent.right
			height:			25

			text:			aboutModel.copyrightMessage
			color:			jaspTheme.textEnabled
		}

		Label
		{
			id:				jaspVersionLabel
			anchors.left:	parent.left
			anchors.top:	copyrightMessage.bottom
			height:			25
			width:			aboutInfoBox.labelwidth

			text:			qsTr("Version:")
			color:			labelcolor
			font.bold:		true

			Text
			{
				id:				jaspVersionText

				text:			aboutModel.version
				anchors.left:	parent.right
				anchors.top:	parent.top
				height:			25
				color:			jaspTheme.textEnabled
			}
		}

		Label
		{
			id:				buildDateLabel
			anchors.left:	parent.left
			anchors.top:	jaspVersionLabel.bottom
			height:			25
			width:			aboutInfoBox.labelwidth

			text:			qsTr("Built on:")
			color:			labelcolor
			font.bold:		true

			Text
			{
				id:				buildDateText
				anchors.left:	parent.right
				anchors.top:	parent.top
				height:			25
				color:			jaspTheme.textEnabled

				text:			aboutModel.buildDate
			}
		}

		Label
		{
			id:				sourceLabel
			anchors.left:	parent.left
			anchors.top:	buildDateLabel.bottom
			height:			25
			width:			aboutInfoBox.labelwidth

			text:			qsTr("Source:")
			color:			labelcolor
			font.bold:		true

			Text
			{
				id:				commitLink
				anchors.left:	parent.right
				anchors.top:	parent.top
				height:			25


				text:			qsTr("Access the sources here")
				color:			jaspTheme.blue
				font.underline:	true

				MouseArea
				{
					id:				mouseAreaSources
					anchors.fill:	parent
					onClicked:		Qt.openUrlExternally(aboutModel.commitUrl)
					cursorShape:	Qt.PointingHandCursor
				}
			}
		}

		Label
		{
			id:				downloadLabel
			anchors.left:	parent.left
			anchors.top:	sourceLabel.bottom
			height:			25
			width:			aboutInfoBox.labelwidth

			text:			qsTr("Download:")
			color:			labelcolor
			font.bold:		true

			Text
			{
				id:				downloadText
				anchors.left:	parent.right
				anchors.top:	parent.top
				height:			25

				text:			aboutModel.downloadUrl
				color:			jaspTheme.blue
				font.underline:	true

				MouseArea
				{
					id:				mouseAreaDownload
					anchors.fill:	parent
					onClicked:		Qt.openUrlExternally(aboutModel.downloadUrl)
					cursorShape:	Qt.PointingHandCursor

				}
			}
		}

		Label
		{
			id:				citationLabel
			anchors.left:	parent.left
			anchors.top:	downloadLabel.bottom
			height:			20
			width:			aboutInfoBox.labelwidth

			text:			qsTr("Citation:")
			color:			labelcolor
			font.bold:		true

			TextArea
			{
				id:				citationText
				anchors.left:	parent.right
				anchors.top:	parent.top
				height:			20
				leftPadding:	0
				topPadding:		0
				bottomPadding:	0

				font.bold:		false
				textFormat:		Text.StyledText
				text:			aboutModel.citation
				color:			jaspTheme.textEnabled

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

			Text
			{
				id:				bibTexText
				anchors.left:	citationLabel.right
				anchors.top:	citationText.bottom
				height:			15

				text:			qsTr("BibTeX")
				color:			jaspTheme.blue
				font.underline:	true

				MouseArea
				{
					id:				mouseAreaBibTex
					anchors.fill:	parent
					onClicked:		Qt.openUrlExternally(aboutModel.citationUrl)
					cursorShape:	Qt.PointingHandCursor
				}
			}
		}

		Text
		{
			id:				warrantyText
			height:			75
			text:			aboutModel.warranty
			textFormat:		Text.StyledText
			opacity:		0.5
			color:			jaspTheme.textEnabled

			anchors
			{
				top:		citationLabel.bottom
				left:		parent.left
				right:		parent.right
				topMargin:	30
			}
		}
	}

	Rectangle
	{
		id:				closeButton
		x:				aboutWindow.width / 2 - parent.x -width / 2
		color:			closebuttoncolor
		radius:			5
		anchors.bottom:	bottomWave.top
		height:			25
		width:			75

		Text
		{
			anchors.centerIn:		parent
			text:					qsTr("Close")
			color:					jaspTheme.white
			horizontalAlignment:	Text.AlignHCenter
			verticalAlignment:		Text.AlignVCenter
		}

		MouseArea
		{
			onClicked:				aboutModel.visible = false
			anchors.fill:			parent
			cursorShape:			Qt.PointingHandCursor
			focus:					true
			Keys.onEnterPressed:	aboutModel.visible = false
			Keys.onReturnPressed:	aboutModel.visible = false
			Keys.onEscapePressed:	aboutModel.visible = false
		}
	}

	Image
	{
		id:					bottomWave

		anchors.bottom:		parent.bottom
		anchors.left:		parent.left
		anchors.right:		parent.right
		height:				parent.height/4
		source:				jaspTheme.iconPath + "jasp-wave-up-green-120.svg"
	}
}

