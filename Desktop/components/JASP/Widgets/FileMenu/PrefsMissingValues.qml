import QtQuick
import QtQuick.Controls as QTC
import JASP.Controls


FocusScope
{
	id:		missingValuesWidget
	property bool	showTitle:		true
	property var	model:			preferencesModel

	implicitWidth:		400 * preferencesModel.uiScale
	implicitHeight: 	250 * preferencesModel.uiScale

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
			width:			(parent.width - 2 * jaspTheme.generalAnchorMargin) / 2
			clip:			true
			anchors
			{
				top:		showTitle ? missingValuesTitle.bottom : parent.top
				bottom:		parent.bottom
				left:		parent.left
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

		Item
		{
			id:				addValueItem
			height:			addButton.height
			anchors
			{
				top:		valuesRectangle.top
				left:		valuesRectangle.right
				right:		parent.right
				margins:	jaspTheme.generalAnchorMargin
				topMargin:	0
			}

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
			id:					resetButton
			text:				qsTr("Reset")
			onClicked:			missingValuesWidget.model.resetEmptyValues()

			anchors
			{
				top:			addValueItem.bottom
				left:			valuesRectangle.right
				right:			parent.right
				margins:		jaspTheme.generalAnchorMargin
			}
		}
	}
}
