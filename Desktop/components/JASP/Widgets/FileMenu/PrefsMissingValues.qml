import QtQuick			2.11
import QtQuick.Controls 2.4
import JASP.Widgets		1.0



Rectangle
{
	width:			valuesRectangle.width + (valuesRectangle.anchors.margins * 2)
	height:			resetButton.y + resetButton.height + missingValuesTitle.anchors.margins

	border.color:	jaspTheme.grayLighter
	border.width:	1
	color:			jaspTheme.uiBackground

	property alias firstComponent:	missingValuesList
	property var   navigateAfter:	undefined

	Text
	{
		id:		missingValuesTitle
		text:	qsTr("Missing Value List")
		color:	jaspTheme.textEnabled
		font:	jaspTheme.font

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
		height:			200 * preferencesModel.uiScale
		width:			200 * preferencesModel.uiScale
		clip:			true
		anchors
		{
			top:		missingValuesTitle.bottom
			left:		parent.left
			margins:	jaspTheme.generalAnchorMargin
		}

		ListView
		{
			id:					missingValuesList
			focus:				true
			boundsBehavior:		Flickable.StopAtBounds
			anchors.fill:		parent
			anchors.margins:	jaspTheme.generalAnchorMargin
			model:				preferencesModel.missingValues
			KeyNavigation.tab:	missingValueToAddText
			KeyNavigation.down:	missingValueToAddText
			delegate:			MenuButton
				{
					id:					hoverphonic
					width:				missingValuesList.width
					text:				modelData
					centerText:			false
					toolTip:			qsTr("Remove missing value")
					onClicked:			preferencesModel.removeMissingValue(modelData)

					Image
					{
						height:				parent.height
						width:				height
						anchors.right:		parent.right
						anchors.top:		parent.top
						source:				jaspTheme.iconPath + "/subtraction-sign-small.svg"
						sourceSize.width:	width * 2
						sourceSize.height:	height * 2
						visible:			parent.hovered
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
			top:		valuesRectangle.bottom
			left:		parent.left
			right:		parent.right
			margins:	jaspTheme.generalAnchorMargin
		}

		Rectangle
		{
			anchors
			{
				left:			parent.left
				right:			addButton.left
				rightMargin:	jaspTheme.generalAnchorMargin
				top:			parent.top
				bottom:			parent.bottom
			}

			color:				jaspTheme.white
			border.color:		jaspTheme.buttonBorderColor
			border.width:		1

			TextInput
			{
				id:					missingValueToAddText
				text:				""
				clip:				true
				font:				jaspTheme.font
				color:				jaspTheme.textEnabled
				KeyNavigation.tab:	addButton
				KeyNavigation.down:	addButton
				onAccepted:			addButton.clicked();
				anchors
				{
					left:			parent.left
					right:			parent.right
					verticalCenter:	parent.verticalCenter
					margins:		jaspTheme.generalAnchorMargin
				}
			}
		}


		RoundedButton
		{
			id:					addButton
			iconSource:			jaspTheme.iconPath + "/addition-sign-small.svg"
			anchors.top:		parent.top
			anchors.right:		parent.right
			KeyNavigation.tab:	resetButton
			KeyNavigation.down:	resetButton

			onClicked:
			{
				preferencesModel.addMissingValue(missingValueToAddText.text)
				missingValueToAddText.text = ""
			}
		}
	}

	RoundedButton
	{
		id:					resetButton
		text:				qsTr("Reset")
		onClicked:			preferencesModel.resetMissingValues()
		KeyNavigation.tab:	navigateAfter
		KeyNavigation.down:	navigateAfter
		anchors
		{
			top:			addValueItem.bottom
			left:			parent.left
			right:			parent.right
			margins:		jaspTheme.generalAnchorMargin
		}
	}
}
