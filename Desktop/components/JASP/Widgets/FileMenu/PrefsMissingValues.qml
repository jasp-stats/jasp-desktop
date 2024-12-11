import QtQuick
import QtQuick.Controls as QTC
import JASP.Controls
import QtQuick.Layouts

FocusScope
{
	id:		missingValuesWidget
	property var	model:						preferencesModel
	property bool	showTitle:					true
	property bool	splitMe:					splitHeight > height
	property bool	showResetWorkspaceButton:	false
	property bool	showWorkspaceMissingValues:	true

	property string resetButtonLabel:			qsTr("Reset")
	property string resetButtonTooltip
	property int	splitHeight:				200 * preferencesModel.uiScale

	implicitWidth:		(splitMe ? 600 : 300) * preferencesModel.uiScale
	implicitHeight: 	300 * preferencesModel.uiScale


	Rectangle
	{

		border.color:	jaspTheme.grayLighter
		border.width:	1
		color:			jaspTheme.uiBackground
		anchors.fill:	parent

		Text
		{
			id:			missingValuesTitle
			text:		qsTr("Missing Value List")
			color:		jaspTheme.textEnabled
			font:		jaspTheme.font
			visible:	showTitle

			anchors
			{
				top:		parent.top
				left:		parent.left
				right:		parent.right
				margins:	jaspTheme.generalAnchorMargin
			}
		}

		Rectangle
		{
			id:				valuesRectangle
			color:			jaspTheme.white
			border.color:	jaspTheme.grayLighter
			clip:			true
			anchors
			{
				top:		showTitle ? missingValuesTitle.bottom : parent.top
				bottom:		splitMe ? parent.bottom : buttons.top
				left:		parent.left
				right:		splitMe ? parent.horizontalCenter : parent.right
				margins:	jaspTheme.generalAnchorMargin
			}

			ListView
			{
				id:					missingValuesList
				boundsBehavior:		Flickable.StopAtBounds
				anchors.fill:		parent
				anchors.margins:	jaspTheme.generalAnchorMargin
				model:				missingValuesWidget.model.emptyValues

				delegate:			Rectangle
				{
					width:				missingValuesList.width
					height:				20  * jaspTheme.uiScale
					color:				enabled && hoverphonic.containsMouse ? jaspTheme.itemHoverColor : "transparent"

					MouseArea
					{
						id:				hoverphonic
						anchors.fill:	parent
						hoverEnabled:	true

						Text
						{
							anchors.left:			parent.left
							anchors.leftMargin:		jaspTheme.generalAnchorMargin
							anchors.verticalCenter:	parent.verticalCenter
							text:					modelData
						}

						MouseArea
						{
							height:				parent.height
							width:				height
							anchors.right:		parent.right
							anchors.top:		parent.top
							onClicked:			missingValuesWidget.model.removeEmptyValue(modelData)
							cursorShape:		Qt.PointingHandCursor
							hoverEnabled:		true

							QTC.ToolTip.text:		qsTr("Remove missing value")
							QTC.ToolTip.visible:	containsMouse

							Image
							{
								anchors.fill:		parent
								source:				jaspTheme.iconPath + "/subtraction-sign-small.svg"
								visible:			enabled && hoverphonic.containsMouse
							}
						}
					}
				}
			}
		}

		ColumnLayout
		{
			id:		buttons

			anchors
			{
				left:		splitMe ? parent.horizontalCenter : parent.left
				right:		parent.right
				bottom:		parent.bottom
				margins:	jaspTheme.generalAnchorMargin
			}

			Item
			{
				id:					addValueItem
				height:				implicitHeight
				implicitHeight:		addButton.height
				Layout.fillWidth:	true


				TextField
				{
					id:					missingValueToAddText
					control.onAccepted:	addButton.clicked();
					focus:				true
					anchors
					{
						right:			addButton.left
						rightMargin:	jaspTheme.generalAnchorMargin
					}
					fieldWidth:			parent.width - addButton.width - jaspTheme.generalAnchorMargin
					KeyNavigation.tab:	addButton
				}

				RoundedButton
				{
					id:					addButton
					iconSource:			jaspTheme.iconPath + "/addition-sign-small.svg"
					height:				missingValueToAddText.height + 2
					width:				height
					anchors.top:		parent.top
					anchors.topMargin:	-1
					anchors.right:		parent.right
					toolTip:			qsTr("Add a missing value")

					KeyNavigation.tab:	resetButton

					onClicked:
					{
						if (missingValueToAddText.control.text)
						{
							missingValuesWidget.model.addEmptyValue(missingValueToAddText.control.text)
							missingValueToAddText.control.text = ""
						}
					}
				}
			}

			RoundedButton
			{
				Layout.fillWidth:	true
				id:					setWorkspaceButton
				text:				qsTr("Set current workspace with these values")
				toolTip:			qsTr("Set the current workspace missing values with these values")
				onClicked:			mainWindow.setDefaultWorkspaceEmptyValues()
				visible:			mainWindow.dataAvailable && showResetWorkspaceButton

			}

			RoundedButton
			{
				Layout.fillWidth:	true
				id:					resetButton
				text:				resetButtonLabel
				toolTip:			resetButtonTooltip
				onClicked:			missingValuesWidget.model.resetEmptyValues()
			}

			RoundedButton
			{
				Layout.fillWidth:	true
				id:					workspaceMissingValuesButton
				text:				qsTr("Show workspace missing values")
				toolTip:			qsTr("Opens the settings for workspace missing values")
				onClicked:			mainWindowRoot.showWorkspaceMenu()
				visible:			showWorkspaceMissingValues && mainWindow.dataAvailable
				isLink:				true
			}
		}
	}
}
