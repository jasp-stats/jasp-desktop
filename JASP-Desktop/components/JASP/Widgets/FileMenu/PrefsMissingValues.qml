import QtQuick			2.11
import QtQuick.Controls 2.4
import JASP.Widgets		1.0
import JASP.Theme		1.0


Rectangle
{
	width:			valuesRectangle.width + (valuesRectangle.anchors.margins * 2)
	height:			resetButton.y + resetButton.height + missingValuesTitle.anchors.margins

	border.color:	Theme.grayLighter
	border.width:	1
	color:			Theme.uiBackground

	property alias firstComponent: missingValuesList

	Text
	{
		id:		missingValuesTitle
		text:	"Missing Value List"
		font:	Theme.font

		anchors
		{
			top:		parent.top
			left:		parent.left
			right:		parent.right
			margins:	Theme.generalAnchorMargin
		}
	}

	Rectangle
	{
		id:				valuesRectangle
		color:			Theme.white
		border.color:	Theme.grayLighter
		height:			200 * preferencesModel.uiScale
		width:			200 * preferencesModel.uiScale
		clip:			true
		anchors
		{
			top:		missingValuesTitle.bottom
			left:		parent.left
			margins:	Theme.generalAnchorMargin
		}

		ListView
		{
			id:					missingValuesList
			focus:				true
			anchors.fill:		parent
			anchors.margins:	Theme.generalAnchorMargin
			model:				preferencesModel.missingValues
			KeyNavigation.tab:	missingValueToAddText
			KeyNavigation.down:	missingValueToAddText
			delegate:			MenuButton
				{
					id:					hoverphonic
					width:				missingValuesList.width
					text:				modelData
					centerText:			false
					toolTip:			"Remove missing value"
					onClicked:			preferencesModel.removeMissingValue(modelData)

					Image
					{
						height:				parent.height
						width:				height
						anchors.right:		parent.right
						anchors.top:		parent.top
						source:				"qrc:/icons/subtraction-sign-small.svg"
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
			margins:	Theme.generalAnchorMargin
		}

		Rectangle
		{
			anchors
			{
				left:			parent.left
				right:			addButton.left
				rightMargin:	Theme.generalAnchorMargin
				top:			parent.top
				bottom:			parent.bottom
			}

			color:				Theme.white
			border.color:		Theme.buttonBorderColor
			border.width:		1

			TextInput
			{
				id:					missingValueToAddText
				text:				""
				clip:				true
				font:				Theme.font
				color:				Theme.textEnabled
				KeyNavigation.tab:	addButton
				KeyNavigation.down:	addButton
				onAccepted:			addButton.clicked();
				anchors
				{
					left:			parent.left
					right:			parent.right
					verticalCenter:	parent.verticalCenter
					margins:		Theme.generalAnchorMargin
				}
			}
		}


		RectangularButton
		{
			id:					addButton
			iconSource:			"qrc:/icons/addition-sign-small.svg"
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

	RectangularButton
	{
		id:					resetButton
		text:				"Reset"
		onClicked:			preferencesModel.resetMissingValues()
		KeyNavigation.tab:	synchronizeDataSave //PrefsData!
		KeyNavigation.down:	synchronizeDataSave
		anchors
		{
			top:			addValueItem.bottom
			left:			parent.left
			right:			parent.right
			margins:		Theme.generalAnchorMargin
		}
	}
}
