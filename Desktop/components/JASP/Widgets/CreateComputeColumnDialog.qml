import QtQuick 2.9
import QtQuick.Controls 2.2


Popup
{
	id:			popupCreateComputedColumn;
	modal:		true;

	y:			(parent.height / 2) - (height / 2)
	x:			(parent.width / 2) - (width / 2)
	width:		popupLoader.width
	height:		popupLoader.height+1

	property bool computeTypeIsJson: true

	background: Item{}
	padding:	0

	Loader
	{
		id:					popupLoader
		sourceComponent:	visible ? computedComponent : null
		visible:			popupCreateComputedColumn.opened
	}

	Component
	{
		id:	computedComponent

		Rectangle
		{
			id:				rootCreateComputedColumn
			height:			childrenRect.height + 20
			width:			Math.max(computeColumnIconRow.width, title.width) + 20
			color:			jaspTheme.uiBackground
			border.color:	jaspTheme.uiBorder
			border.width:	1

			Component.onCompleted:
			{
				labelModel.visible = false;
				nameEdit.forceActiveFocus();
			}

			function createComputedColumn()
			{
				if(!isNaN(nameEdit.text)) //Its a number!
					nameEdit.text = 'V' + nameEdit.text

				if(computedColumnsInterface.isColumnNameFree(nameEdit.text))
				{
					computedColumnsInterface.createComputedColumn(nameEdit.text, rootCreateComputedColumn.selectedColumnType, popupCreateComputedColumn.computeTypeIsJson)
					focus = true
					popupCreateComputedColumn.close()
				}
				else
					nameEdit.lastCheckedColumnNameInUse = nameEdit.text;
			}

			Text
			{
				id:					title
				text:				qsTr("Create Computed Column")
				font:				jaspTheme.fontGroupTitle
				color:				jaspTheme.textEnabled
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
				height:	marge * 2 + (nameEdit.implicitHeight * 2)

				property real marge: 10 * preferencesModel.uiScale

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
					text:					qsTr("Name:")
					font:					jaspTheme.font
					color:					jaspTheme.textEnabled
					anchors.left:			parent.left
					anchors.verticalCenter: parent.verticalCenter
					verticalAlignment:		Text.AlignVCenter
				}

				Rectangle
				{
					id:					nameBox
					color:				jaspTheme.white
					border.color:		jaspTheme.black
					border.width:		1

					anchors
					{
						top:		parent.top
						left:		nameLabel.right
						right:		parent.right
						bottom:		parent.bottom
						margins:	6 * preferencesModel.uiScale
					}
					
					ScrollView
					{
						property double textHeightResidual: parent.height - nameEdit.implicitHeight
						
						clip:					true
						anchors.fill:			parent
						anchors.margins:		4 * preferencesModel.uiScale
						anchors.topMargin:		textHeightResidual / 2
						onContentWidthChanged:	if(contentWidth >= width) ScrollBar.horizontal.position = 1 - ScrollBar.horizontal.size
						
						TextEdit
						{
							property string defaultText:				"..."
							property string lastCheckedColumnNameInUse: ""
							property bool	columnNameInUse:			lastCheckedColumnNameInUse !== "" && lastCheckedColumnNameInUse === text
							property bool	validEntry:					text != defaultText && text.length > 0 && !columnNameInUse
	
							id:						nameEdit
							text:					defaultText
							font:					jaspTheme.font
							color:					columnNameInUse ? jaspTheme.red : jaspTheme.black
							width:					Math.max(implicitWidth, nameBox.width)
							selectByMouse:			true
	
							ToolTip.delay:			0
							ToolTip.timeout:		10000
							ToolTip.visible:		columnNameInUse
							ToolTip.text:			qsTr("Column name is already used, please choose a different one.")
	
							Keys.onReturnPressed: (event)=>	rootCreateComputedColumn.createComputedColumn()
		
							onActiveFocusChanged:
							{
								if( activeFocus	&& text === defaultText	) text = ""
								if(!activeFocus && text === ""			) text = defaultText
							}
						}
						
					}
					
					MouseArea
					{
						z:					1234
						anchors.fill:		parent
						hoverEnabled:		true
						acceptedButtons:	Qt.NoButton
						cursorShape:		Qt.IBeamCursor
					}
				}
			}



			Item
			{
				id: computeTypeSelector

				anchors.top:				nameItem.bottom
				anchors.topMargin:			10
				anchors.horizontalCenter:	parent.horizontalCenter
				height:						45 * preferencesModel.uiScale

				RoundedButton
				{
					id:						rCodeSelectah

					anchors.top:			parent.top
					//anchors.left:			parent.left
					anchors.right:			parent.horizontalCenter
					anchors.bottom:			parent.bottom
					anchors.rightMargin:	5

					iconSource:				jaspTheme.iconPath + "/R.png"
					onClicked:				popupCreateComputedColumn.computeTypeIsJson = false
					selected:				!popupCreateComputedColumn.computeTypeIsJson

					width:					height

					toolTip:				qsTr("Define column through R code")
				}

				RoundedButton
				{
					id:					jsonSelectah

					anchors.top:		parent.top
					anchors.left:		parent.horizontalCenter
					//anchors.right:	parent.right
					anchors.bottom:		parent.bottom
					anchors.leftMargin:	5

					iconSource:			jaspTheme.iconPath + "/NotR.png"
					onClicked:			popupCreateComputedColumn.computeTypeIsJson = true
					selected:			popupCreateComputedColumn.computeTypeIsJson

					width:				height

					toolTip:			qsTr("Define column through drag and drop formulas")
				}

			}

			property int selectedColumnType: columnTypeScale
			Row
			{
				id:			computeColumnIconRow
				height:		25 * preferencesModel.uiScale
				spacing:	jaspTheme.generalAnchorMargin

				anchors.top:				computeTypeSelector.bottom
				anchors.topMargin:			10
				anchors.horizontalCenter:	parent.horizontalCenter

				Repeater
				{
					id:		iconRepeater
					model:	[columnTypeScale, columnTypeOrdinal, columnTypeNominal, columnTypeNominalText] //these are set in the rootcontext in mainwindow!

					Rectangle
					{
						id:				columnTypeChangeIcon
						width:			iconAndTextCreateComputeColumn.width + iconAndTextCreateComputeColumn.anchors.leftMargin + popupText.anchors.leftMargin + 4
						height:			computeColumnIconRow.height
						color:			iAmSelected ? jaspTheme.buttonColorPressed : popupIconComputeMouseArea.useThisColor
						border.color:	iAmSelected ? jaspTheme.buttonBorderColorHovered : jaspTheme.buttonBorderColor
						border.width:	1
						radius:			jaspTheme.borderRadius

						property bool iAmSelected: rootCreateComputedColumn.selectedColumnType === iconRepeater.model[index]

						Item
						{
							id:					iconAndTextCreateComputeColumn
							width:				(popupIconComputeImage.width + popupText.width)
							height:				computeColumnIconRow.height * 0.5
							anchors
							{
								verticalCenter:	parent.verticalCenter
								left:			parent.left
								leftMargin:		4
							}

							Image
							{
								id:						popupIconComputeImage

								anchors.verticalCenter: parent.verticalCenter

								source:					jaspTheme.iconPath + dataSetModel.getColumnTypesWithCorrespondingIcon()[iconRepeater.model[index]]
								width:					height
								height:					parent.height
								sourceSize.width:		width
								sourceSize.height:		height
							}

							Text
							{
								id:						popupText
								text:					iconRepeater.model[index] === columnTypeScale ? qsTr("Scale") : ( iconRepeater.model[index] === columnTypeOrdinal ? qsTr("Ordinal") :  iconRepeater.model[index] === columnTypeNominal ? qsTr("Nominal") : qsTr("Text"))
								font:					jaspTheme.font
								color:					jaspTheme.textEnabled
								anchors.left:			popupIconComputeImage.right
								anchors.verticalCenter: parent.verticalCenter
								anchors.leftMargin:		4
							}
						}

						MouseArea
						{
							id:				popupIconComputeMouseArea
							anchors.fill:	parent
							hoverEnabled:	true
							cursorShape:	Qt.PointingHandCursor

							property color useThisColor: containsMouse ? jaspTheme.buttonColorHovered : jaspTheme.buttonColor

							onClicked:
							{
								focus = true
								rootCreateComputedColumn.selectedColumnType = iconRepeater.model[index]
							}
						}
					}
				}
			}

			RoundedButton
			{
				id:				helpButton
				iconSource:		jaspTheme.iconPath + "info-button.png"
				width:			height
				height:			createButton.height
				onClicked:		helpModel.showOrTogglePage("other/ComputedColumns");
				toolTip:		qsTr("Open Documentation")
				anchors
				{
					left:		parent.left
					top:		computeColumnIconRow.bottom
					margins:	jaspTheme.generalAnchorMargin
				}
			}

			RoundedButton
			{
				id:			createButton
				text:		qsTr("Create Column")
				enabled:	nameEdit.validEntry
				toolTip:	nameEdit.validEntry ? qsTr("Click here to create your new computed column '%1'").arg(nameEdit.text) : qsTr("Enter a valid (unused) name for computed column")
				onClicked:	rootCreateComputedColumn.createComputedColumn()
				anchors
				{
					top:		computeColumnIconRow.bottom
					margins:	jaspTheme.generalAnchorMargin
					left:		helpButton.right
					right:		closeButtonCross.left
				}
			}

			RoundedButton
			{
				id:				closeButtonCross
				iconSource:		jaspTheme.iconPath + "cross.png"
				width:			height
				height:			createButton.height
				onClicked:		popupCreateComputedColumn.close()
				toolTip:		qsTr("Close without creating a computed column")
				anchors
				{
					right:		parent.right
					top:		computeColumnIconRow.bottom
					margins:	jaspTheme.generalAnchorMargin
				}
			}
		}
	}
}
