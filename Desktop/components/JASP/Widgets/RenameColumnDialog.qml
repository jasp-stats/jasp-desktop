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
				value:					dataSetModel.columnName(popupRenameColumnDialog.colIndex)

				onEditingFinished: {
					renameButton.clicked();
				}

				KeyNavigation.tab:			renameButton
				KeyNavigation.backtab:		cancelButton
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

					KeyNavigation.tab:		columnName
					KeyNavigation.backtab:	columnName

				}

				JW.RoundedButton
				{
					id:						renameButton
					activeFocusOnTab:		true
					text:					qsTr("Rename")

					onClicked: { 
						dataSetModel.setColumnName(popupRenameColumnDialog.colIndex, columnName.value); 
						popupRenameColumnDialog.close(); 
					}

					KeyNavigation.tab:			columnName
					KeyNavigation.backtab:		cancelButton
				}
			}

		}
	}
}
