import QtQuick			2.15
import QtQuick.Controls 2.15
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
		cols.forceActiveFocus();
	}

	contentItem: Item
	{
		width:	columnName.width + 3 * popupRenameColumnDialog.padding
		height:	title.height + columnName.height + renameButton.height + 3 * popupRenameColumnDialog.padding

		Text
		{
			id:					title
			text:				qsTr("Rename column")
			font:				jaspTheme.fontGroupTitle
			color:				jaspTheme.textEnabled

			anchors
			{
				top:	parent.top
				left:	parent.left
			}
		}


		JC.TextField
		{
			id:						columnName
			value:					dataSetModel.columnName(popupRenameColumnDialog.colIndex)
			// color:					jaspTheme.textEnabled
			// selectByMouse:			true
			fieldWidth:					250
			anchors
			{
				top:				title.bottom
				left:				contentItem.left
				right:				contentItem.right
				topMargin:			jaspTheme.generalAnchorMargin
				bottomMargin:		jaspTheme.generalAnchorMargin
				rightMargin:		jaspTheme.generalAnchorMargin
			}

			Keys.onEnterPressed:	renameButton.clicked();

			KeyNavigation.tab:			renameButton
			KeyNavigation.backtab:		cancelButton
		}


        JW.RoundedButton
		{
			id:						cancelButton
			activeFocusOnTab:		true
			text:					qsTr("Cancel")

			onClicked:				popupRenameColumnDialog.close()

			KeyNavigation.tab:		columnName
			KeyNavigation.backtab:	columnName

			anchors
			{
				right:				renameButton.left
				bottom:				renameButton.bottom

				rightMargin:		jaspTheme.generalAnchorMargin
			}
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

			anchors
			{
				top:				columnName.bottom
				right:				contentItem.right
				
				topMargin:			jaspTheme.generalAnchorMargin
				leftMargin:			jaspTheme.generalAnchorMargin
			}
		}
	}
}
