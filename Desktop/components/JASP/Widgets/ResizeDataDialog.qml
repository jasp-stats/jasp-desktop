import QtQuick			2.15
import QtQuick.Controls 2.15
import QtQuick.Layouts 	1.15
import JASP.Controls	1.0 as JC
import JASP.Widgets		1.0 as JW

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
		radius:  		10
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
		width:	layout.width + 2 * popupResizeData.padding
		height: layout.height + 2 * popupResizeData.padding

		ColumnLayout {
			id: layout
			spacing: 2 * jaspTheme.generalAnchorMargin

			Layout.fillWidth: true
			Layout.maximumWidth: groupBox.content.width

			Text
			{
				id:					title
				text:				qsTr("Table Size")
				font:				jaspTheme.fontGroupTitle
				color:				jaspTheme.textEnabled
			}

			JC.Group {
				id: groupBox

				JC.IntegerField
				{
					id:						cols
					label:					qsTr("Number of columns")
					value:					dataSetModel.columnCount()
					min:					1

					KeyNavigation.tab:		rows
					KeyNavigation.backtab:	cols

					Keys.onEnterPressed:	resizeButton.clicked();
					Keys.onReturnPressed:	resizeButton.clicked();
				}

				JC.IntegerField
				{
					id:						rows
					label:					qsTr("Number of rows")
					value:					dataSetModel.rowCount()
					min:					1

					KeyNavigation.tab:		rows
					KeyNavigation.backtab:	cols

					Keys.onEnterPressed:	resizeButton.clicked();
					Keys.onReturnPressed:	resizeButton.clicked();
				}

			}


			RowLayout {
				spacing: jaspTheme.generalAnchorMargin

				Layout.alignment: Qt.AlignRight

				JW.RoundedButton
				{
					id:						cancelButton
					activeFocusOnTab:		true
					text:					qsTr("Cancel")
					onClicked:				popupResizeData.close()
				}

				JW.RoundedButton
				{
					id:						resizeButton
					activeFocusOnTab:		true
					text:					qsTr("Resize")

                    onClicked: {
                        if (rows.value < dataSetModel.rowCount() || cols.value < dataSetModel.columnCount()) {
                            yesNoDialog.open();
                        } else {
                            dataSetModel.resizeData(rows.value, cols.value);
                            popupResizeData.close();
                        }

                    }
				}

			}
		}
	}

    JW.YesNoDialog {
		id: yesNoDialog
        title: qsTr("Would you like to remove some of the rows or columns of the data set?")
		text: qsTr("The provided rows and columns are smaller than the rows or columns currently present in the data set. This will remove some rows or columns. Please be aware that once the data set is shrunk the lost data cannot be retrieved.")

        onYes: {
            dataSetModel.resizeData(rows.value, cols.value);
            popupResizeData.close();
        }

	}
}
