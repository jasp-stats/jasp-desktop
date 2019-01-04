import QtQuick 2.9
import QtQuick.Controls 2.2
import JASP.Theme 1.0

Popup {
	id: popupCreateComputedColumn;
	modal: true; focus: true;

	y: (parent.height / 2) - (height / 2)
	x: (parent.width / 2) - (width / 2)

	closePolicy: Popup.CloseOnPressOutside | Popup.CloseOnEscape
	property bool computeTypeIsJson: true

	background: Item{}
	padding:	0
	width:		rootCreateComputedColumn.width
	height:		rootCreateComputedColumn.height+1

	function reset()
	{
		rootCreateComputedColumn.selectedColumnType = columnTypeScale
		nameEdit.reset()
	}

	function createComputedColumn()
	{
		if(computedColumnsInterface.isColumnNameFree(nameEdit.text))
		{
			computedColumnsInterface.createComputedColumn(nameEdit.text, rootCreateComputedColumn.selectedColumnType, popupCreateComputedColumn.computeTypeIsJson)
			focus = true
			popupCreateComputedColumn.close()
			if(!computeColumnWindow.changed)
				computeColumnWindow.open(nameEdit.text);
		}
		else
			nameEdit.lastCheckedColumnNameInUse = nameEdit.text;
	}

	onOpened: reset()

	Rectangle
	{
		id: rootCreateComputedColumn

		//width: computeColumnIconRow.width
		height: childrenRect.height + 20
		width: Math.max(computeColumnIconRow.width, title.width) + 20
		color: "#EEEEEE"
		//radius: 20
		border.color: Theme.black
		border.width: 1

		Text
		{
			id:					title
			text:				"Create Computed Column"
			font.bold:			true
			font.pixelSize:		20 * ppiScale
			verticalAlignment:	Text.AlignVCenter
			anchors
			{
				top:				parent.top
				topMargin:			10
				horizontalCenter:	parent.horizontalCenter
			}
		}

		Item
		{
			id:		nameItem
			property real marge: 10 * ppiScale
			height:	marge * 2 + (baseFontSize * 1.5 * ppiScale)

			anchors
			{
				top:			title.bottom
				left:			parent.left
				right:			parent.right
				topMargin:		marge
				leftMargin:		marge
				rightMargin:	marge
			}


			Text
			{
				id:						nameLabel
				text:					"Name:"
				anchors.left:			parent.left
				anchors.verticalCenter: parent.verticalCenter
				verticalAlignment:		Text.AlignVCenter
			}

			Rectangle
			{
				id:					nameBox
				anchors.top:		parent.top
				anchors.left:		nameLabel.right
				anchors.right:		parent.right
				anchors.bottom:		parent.bottom
				anchors.margins:	6 * ppiScale

				color:				Theme.white
				border.color:		Theme.black
				border.width:		1

				TextEdit
				{
					id: nameEdit
					property string defaultText: "..."
					property string lastCheckedColumnNameInUse: ""
					anchors.fill: parent
					anchors.margins: 2

					text: defaultText
					font.pixelSize: baseFontSize * ppiScale
					onActiveFocusChanged:
					{
						if(activeFocus && text === defaultText) text = ""
						if(!activeFocus && text === "") text = defaultText
					}
					property bool columnNameInUse: lastCheckedColumnNameInUse !== "" && lastCheckedColumnNameInUse === text
					property bool validEntry: text != defaultText && text.length > 0 && !columnNameInUse
					color: columnNameInUse ? "red" : Theme.black

					ToolTip.delay: 0
					ToolTip.timeout: 10000
					ToolTip.visible: columnNameInUse
					ToolTip.text: "Column name is already used, please choose a different one."

					Keys.onReturnPressed: popupCreateComputedColumn.createComputedColumn()

					function reset()
					{
						lastCheckedColumnNameInUse = ""
						nameEdit.focus = true
						nameEdit.text = ""
					}

					MouseArea
					{
						anchors.fill: parent
						hoverEnabled: true
						acceptedButtons: Qt.NoButton
						cursorShape: containsMouse ? Qt.IBeamCursor : Qt.ArrowCursor
					}
				}
			}
		}



		Item
		{
			id: computeTypeSelector

			anchors.top:				nameItem.bottom
			anchors.topMargin:			10
			anchors.horizontalCenter:	parent.horizontalCenter
			height:						45 * ppiScale

			RectangularButton
			{
				id:						rCodeSelectah

				anchors.top:			parent.top
				//anchors.left:			parent.left
				anchors.right:			parent.horizontalCenter
				anchors.bottom:			parent.bottom
				anchors.rightMargin:	5

				iconSource:				"qrc:/icons/R.png"
				onClicked:				popupCreateComputedColumn.computeTypeIsJson = false
				selected:				!popupCreateComputedColumn.computeTypeIsJson

				width:					height

				toolTip:				"Define column through R code"
			}

			RectangularButton
			{
				id:					jsonSelectah

				anchors.top:		parent.top
				anchors.left:		parent.horizontalCenter
				//anchors.right:	parent.right
				anchors.bottom:		parent.bottom
				anchors.leftMargin:	5

				iconSource:			"qrc:/icons/NotR.png"
				onClicked:			popupCreateComputedColumn.computeTypeIsJson = true
				selected:			popupCreateComputedColumn.computeTypeIsJson

				width:				height

				toolTip:			"Define column through drag and drop formulas"
			}

		}

		property int selectedColumnType: columnTypeScale
		Row
		{
			id:			computeColumnIconRow
			height:		25 * ppiScale
			spacing:	2

			anchors.top:				computeTypeSelector.bottom
			anchors.topMargin:			10
			anchors.horizontalCenter:	parent.horizontalCenter

			Repeater{
				id:		iconRepeater
				model:	[columnTypeScale, columnTypeOrdinal, columnTypeNominal, columnTypeNominalText] //these are set in the rootcontext in mainwindow!

				Rectangle
				{
					id:			columnTypeChangeIcon

					width:		iconAndTextCreateComputeColumn.width + iconAndTextCreateComputeColumn.anchors.leftMargin + popupText.anchors.leftMargin + 4
					height:		computeColumnIconRow.height
					//radius:	10

					property bool iAmSelected: rootCreateComputedColumn.selectedColumnType === iconRepeater.model[index]
					color: iAmSelected ? Theme.white : popupIconComputeMouseArea.useThisColor

					border.color: iAmSelected ? Theme.black : "lightGray"
					border.width: 1

					Item
					{
						id: iconAndTextCreateComputeColumn
						width: (popupIconComputeImage.width + popupText.width)
						height: computeColumnIconRow.height * 0.5
						anchors.verticalCenter: parent.verticalCenter
						anchors.left: parent.left
						anchors.leftMargin: 4

						Image
						{
							id: popupIconComputeImage

							anchors.verticalCenter: parent.verticalCenter

							source: dataSetModel.getColumnTypesWithCorrespondingIcon()[iconRepeater.model[index]]
							width:	height
							height: parent.height
							sourceSize.width:	width
							sourceSize.height:	height
						}

						Text
						{
							id: popupText
							text: iconRepeater.model[index] === columnTypeScale ? "Scale" : ( iconRepeater.model[index] === columnTypeOrdinal ? "Ordinal" :  iconRepeater.model[index] === columnTypeNominal ? "Nominal" : "Text")

							anchors.left: popupIconComputeImage.right
							anchors.verticalCenter: parent.verticalCenter
							anchors.leftMargin: 4
						}
					}

					MouseArea
					{
						id: popupIconComputeMouseArea
						anchors.fill: parent

						hoverEnabled: true
						cursorShape: Qt.PointingHandCursor

						property color useThisColor: containsMouse ? Theme.white : "lightGray"

						onClicked:
						{
							focus = true
							rootCreateComputedColumn.selectedColumnType = iconRepeater.model[index]
						}
					}
				}
			}
		}

		RectangularButton
		{
			anchors.top: computeColumnIconRow.bottom
			anchors.topMargin: 10
			anchors.horizontalCenter: parent.horizontalCenter

			text:		"Create"
			enabled:	nameEdit.validEntry
			toolTip:	nameEdit.validEntry ? "Click here to create your new computed column '" + nameEdit.text + "'" : "Enter a valid (unused) name for computed column"

			onClicked:	popupCreateComputedColumn.createComputedColumn()
		}
	}
}
