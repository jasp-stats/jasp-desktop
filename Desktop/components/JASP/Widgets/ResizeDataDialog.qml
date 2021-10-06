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
		radius:  10
	}
	padding:	15

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
			width:	inputs.width + 2 * popupResizeData.padding
			height: title.height + inputs.height + okButton.height + 2 * popupResizeData.padding

			Component.onCompleted:	cols.forceActiveFocus();

			Text
			{
				id:					title
                text:				qsTr("Table Size")
				font:				jaspTheme.fontGroupTitle
				color:				jaspTheme.textEnabled

				anchors
				{
					top:			parent.top
					left:			parent.left
				}
			}

			Item
			{
				id:				inputs
				width:			1.25 * groupBox.implicitWidth
				height:			groupBox.implicitHeight

				anchors
				{
					top:		title.bottom
					left:		parent.left
					// right:		parent.right
					margins:	jaspTheme.generalAnchorMargin
				}

				Group {
					id: groupBox

					IntegerField
					{
						id:						cols
						label:					qsTr("Number of columns")
						value:					dataSetModel.columnCount()
						// fieldWidth:				width
						// anchors
						// {
						// 	// top:				x.verticalCenter
						// 	left:				parent.left
						// 	// right:				x.left
						// 	margins:			jaspTheme.generalAnchorMargin
						// }

						//KeyNavigation.tab:		rows
						// KeyNavigation.right:	rows
						// KeyNavigation.down:		okButton
					}

					IntegerField
					{
						id:						rows
						label:					qsTr("Number of rows")
						value:					dataSetModel.rowCount()
						// fieldWidth:				width
						// anchors
						// {
						// 	// top:				x.verticalCenter
						// 	// left:				x.right
						// 	right:				parent.right
						// 	margins:			jaspTheme.generalAnchorMargin
						// }
						// KeyNavigation.down:		okButton
						// KeyNavigation.right:	okButton
						//KeyNavigation.tab:		okButton
					}

				}
			}


			RoundedButton
			{
				id:						cancelButton
				activeFocusOnTab:		true
				text:					qsTr("Cancel")
				// iconSource:				jaspTheme.iconPath + "cross.png"
				// width:					height
				// height:					okButton.height
				onClicked:				popupResizeData.close()
				// toolTip:				qsTr("Close without resizing data")
				// KeyNavigation.up:		rows
				KeyNavigation.tab:		okButton
				KeyNavigation.backtab:	cols
				anchors
				{
					right:				okButton.left
					top:				inputs.bottom
					// topMargin:			jaspTheme.generalAnchorMargin
					leftMargin:			jaspTheme.generalAnchorMargin
					rightMargin:		jaspTheme.generalAnchorMargin
				}
			}

			RoundedButton
			{
				id:						okButton
				activeFocusOnTab:		true
				text:					qsTr("OK")
                buttonPadding:          20
				onClicked:				{ dataSetModel.resizeData(rows.value, cols.value); popupResizeData.close(); }
				// toolTip:				qsTr("Resize data to set values")
				// KeyNavigation.right:	cancelButton
				KeyNavigation.tab:		rows
				KeyNavigation.backtab:	cancelButton
				// KeyNavigation.left:		rows
				// KeyNavigation.up:		cols

				anchors
				{
					top:				inputs.bottom
					right:				inputs.right
					// topMargin:			jaspTheme.generalAnchorMargin
					leftMargin:			jaspTheme.generalAnchorMargin
//					rightMargin:		jaspTheme.generalAnchorMargin
				}
			}
		}
	}
}
