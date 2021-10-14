import QtQuick			2.15
import QtQuick.Controls 2.15
import QtQuick.Layouts 	1.15
import JASP.Controls	1.0 as JC
import JASP.Widgets     1.0 as JW

Popup
{
	id:			popupRenameColumnDialog;
	modal:		true
	padding:	15
	focus:		true

	anchors.centerIn: 	Overlay.overlay
	width:				contentItem.width
	height:				contentItem.height

	property int colIndex;

	background: Rectangle
	{
		color:			jaspTheme.uiBackground
		border.color:	jaspTheme.uiBorder
		border.width:	1
		radius:			10
	}

	Connections
	{
		target:					dataSetModel
		onRenameColumnDialog:
		{
			console.log("renaming column dialog opened for " + String(columnIndex))
			colIndex = columnIndex;
			popupRenameColumnDialog.open()
		}
	}

	onOpened: {
		columnName.forceActiveFocus()
		columnName.value = dataSetModel.columnName(popupRenameColumnDialog.colIndex)
	}

	contentItem: Item
	{
		width:	layout.width + 2 * popupRenameColumnDialog.padding
		height:	layout.height + 2 * popupRenameColumnDialog.padding

		ColumnLayout {
			id:	layout
			spacing: 2 * jaspTheme.generalAnchorMargin

			Layout.fillWidth: true
			Layout.minimumWidth: columnName.width

			anchors
			{
				top:	parent.top
				left:	parent.left
			}

			Text
			{
				id:					title
				text:				qsTr("Rename Column")
				font:				jaspTheme.fontGroupTitle
				color:				jaspTheme.textEnabled

			}

			JC.TextField
			{
				id:						columnName

				validator: RegularExpressionValidator { regularExpression: /^(?!\s*$).+/ }

				Keys.onEnterPressed: if (acceptableInput) renameButton.clicked();
						
				Keys.onReturnPressed: if (acceptableInput) renameButton.clicked();
			}


			RowLayout {
				spacing: jaspTheme.generalAnchorMargin

				Layout.alignment: Qt.AlignRight

				JW.RoundedButton
				{
					id:						cancelButton
					activeFocusOnTab:		true
					text:					qsTr("Cancel")

					onClicked:				popupRenameColumnDialog.close()
				}

				JW.RoundedButton
				{
					id:						renameButton
					activeFocusOnTab:		true
					text:					qsTr("Rename")
                    enabled:				columnName.value.trim()

					onClicked: { 
						if (dataSetModel.columnName(popupRenameColumnDialog.colIndex) == columnName.value) 
						{
							popupRenameColumnDialog.close();
						} else if (dataSetModel.isColumnNameFree(columnName.value))
						{
							dataSetModel.setColumnName(popupRenameColumnDialog.colIndex, columnName.value);
							popupRenameColumnDialog.close();
						} else {
							warningDialog.open();
						}
					}
				}
			}
		}
	}

	JW.WarningDialog {
		id: warningDialog
		title: qsTr("The name '%1' is already taken.").args(columnIndex.value)
		text: qsTr("Please choose a different name.")
	}
}
