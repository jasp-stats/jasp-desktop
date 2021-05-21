import QtQuick			2.15
import QtQuick.Controls 2.15
import JASP.Controls	1.0

Popup
{
    id:			popupResizeData;
	modal:		true;

	y:			(parent.height / 2) - (height / 2)
	x:			(parent.width / 2)  - (width / 2)
	width:		popupLoader.width
	height:		popupLoader.height+1

	background: Rectangle
	{
		color:			jaspTheme.uiBackground
		border.color:	jaspTheme.uiBorder
		border.width:	1
	}
	padding:	0

    Connections
	{
		target:			ribbonModel
		onResizeData:	popupResizeData.open()
    }

	Loader
	{
		id:					popupLoader
		sourceComponent:	visible ? resizeComp : null
		visible:			popupResizeData.opened
	}

	Component
	{
	    id:	resizeComp

		Item
		{
			height:	resizeButton.y + resizeButton.height + jaspTheme.generalAnchorMargin
			width:	inputs.width

			Component.onCompleted:	cols.forceActiveFocus();

			Text
			{
				id:					title
				text:				qsTr("Resize Data")
				font:				jaspTheme.fontGroupTitle
				color:				jaspTheme.textEnabled
				verticalAlignment:	Text.AlignVCenter
				anchors
				{
					top:				parent.top
					topMargin:			jaspTheme.generalAnchorMargin
					horizontalCenter:	parent.horizontalCenter
				}
			}

			Item
			{
				id:				inputs
				width:			200 * jaspTheme.uiScale
				height:			cols.y + cols.height + jaspTheme.generalAnchorMargin

				anchors
				{
					top:		title.bottom
					left:		parent.left
					right:		parent.right
					margins:	jaspTheme.generalAnchorMargin
				}

				Label
				{
					id:				colsLabel
					text:			qsTr("Columns")
					anchors
					{
						top:		inputs.top
						left:		cols.left
						right:		cols.right
					}
				}

				Label
				{
					id:				rowsLabel
					text:			qsTr("Rows")
					anchors
					{
						top:		inputs.top
						left:		rows.left
						right:		rows.right
					}
				}

				Label
				{
					id:						x
					text:					"X"
					horizontalAlignment:	Text.AlignHCenter
					verticalAlignment:		Text.AlignVCenter
					anchors.centerIn:		parent
				}

				IntegerField
				{
					id:						cols
					value:					dataSetModel.columnCount()
					fieldWidth:				width
					anchors
					{
						top:				x.verticalCenter
						left:				parent.left
						right:				x.left
						margins:			jaspTheme.generalAnchorMargin
					}

					//KeyNavigation.tab:		rows
					KeyNavigation.right:	rows
					KeyNavigation.down:		resizeButton
				}

				IntegerField
				{
					id:						rows
					value:					dataSetModel.rowCount()
					fieldWidth:				width
					anchors
					{
						top:				x.verticalCenter
						left:				x.right
						right:				parent.right
						margins:			jaspTheme.generalAnchorMargin
					}
					KeyNavigation.down:		resizeButton
					KeyNavigation.right:	resizeButton
					//KeyNavigation.tab:		resizeButton
				}
			}

			RectangularButton
			{
				id:						resizeButton
				activeFocusOnTab:		true
				text:					qsTr("Resize")
				onClicked:				{ dataSetModel.resizeData(rows.value, cols.value); popupResizeData.close(); }
				toolTip:				qsTr("Resize data to set values")
				KeyNavigation.right:	closeButtonCross
				//KeyNavigation.tab:		closeButtonCross
				//KeyNavigation.backtab:	rows
				KeyNavigation.left:		rows
				KeyNavigation.up:		cols

				anchors
				{
					top:				inputs.bottom
					margins:			jaspTheme.generalAnchorMargin
					left:				parent.left
					right:				closeButtonCross.left
				}
			}

			RectangularButton
			{
				id:						closeButtonCross
				activeFocusOnTab:		true
				iconSource:				jaspTheme.iconPath + "cross.png"
				width:					height
				height:					resizeButton.height
				onClicked:				popupResizeData.close()
				toolTip:				qsTr("Close without resizing data")
				KeyNavigation.up:		rows
				anchors
				{
					right:				parent.right
					top:				inputs.bottom
					margins:			jaspTheme.generalAnchorMargin
				}
			}
		}
	}
}
