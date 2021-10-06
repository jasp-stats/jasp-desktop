import QtQuick			2.15
import QtQuick.Controls 2.15
import JASP.Controls	1.0

Popup
{
    id:			popupResizeData;
	modal:		true
	padding:	15
	focus:		true

	anchors.centerIn: 	Overlay.overlay
	width:				contentItem.width
	height:				contentItem.height

	background: Rectangle
	{
		color:			jaspTheme.uiBackground
		border.color:	jaspTheme.uiBorder
		border.width:	1
		radius:  10
	}

    Connections
	{
		target:			ribbonModel
		onResizeData:	popupResizeData.open()
    }

    onOpened: {
		cols.forceActiveFocus();
	}

	contentItem: Item
	{
		width:	inputs.width + 3 * popupResizeData.padding
		height: title.height + inputs.height + okButton.height + 3 * popupResizeData.padding

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
				margins:	jaspTheme.generalAnchorMargin
			}

			Group {
				id: groupBox

				IntegerField
				{
					id:						cols
					label:					qsTr("Number of columns")
					value:					dataSetModel.columnCount()

					KeyNavigation.tab:		rows
				}

				IntegerField
				{
					id:						rows
					label:					qsTr("Number of rows")
					value:					dataSetModel.rowCount()

					KeyNavigation.tab:		cancelButton
					KeyNavigation.backtab:	cols
				}


			}
		}


		RoundedButton
		{
			id:						cancelButton
			activeFocusOnTab:		true
			text:					qsTr("Cancel")
			onClicked:				popupResizeData.close()

			KeyNavigation.tab:		okButton
			KeyNavigation.backtab:	cols
			anchors
			{
				right:				okButton.left
				bottom:				okButton.bottom
				rightMargin:		jaspTheme.generalAnchorMargin
			}
		}

		RoundedButton
		{
			id:						okButton
			activeFocusOnTab:		true
			text:					qsTr("OK")
            buttonPadding:          20
			
			onClicked: {
				dataSetModel.resizeData(rows.value, cols.value);
				popupResizeData.close(); 
			}

			KeyNavigation.tab:		rows
			KeyNavigation.backtab:	cancelButton
			anchors
			{
				top:				inputs.bottom
				right:				contentItem.right
				topMargin:			jaspTheme.generalAnchorMargin
				leftMargin:			jaspTheme.generalAnchorMargin
			}
		}
	}
}
