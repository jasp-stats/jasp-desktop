import QtQuick			2.13
import QtQuick.Controls 2.13


Dialog
{
					id:			dialogRoot
					title:		qsTr("Changes were made")
	property alias	text:		contentText.text
					modal:		true
					x:			(parent.width - width) / 2
					y:			(parent.height - height) / 2
					width:		mainWindowRoot.width / 2


	signal			save();
	signal			cancel();
	signal			discard();

	header: Item
	{
		implicitWidth:	dialogRoot.width
		implicitHeight:	headerText.height + (2 * jaspTheme.generalAnchorMargin)

		Text
		{
			id:					headerText
			text:				dialogRoot.title
			font:				jaspTheme.fontGroupTitle
			anchors.centerIn:	parent
		}
	}

	contentItem: Item
	{
		implicitWidth:	dialogRoot.width
		implicitHeight:	contentText.height + (2 * jaspTheme.generalAnchorMargin)

		Text
		{
			id:						contentText
			text:					qsTr("There are unapplied changes; what would you like to do?")
			font:					jaspTheme.font
			wrapMode:				Text.WrapAtWordBoundaryOrAnywhere
			horizontalAlignment:	Text.AlignHCenter
			anchors.centerIn:		parent
		}
	}

	footer: Item
	{
		implicitWidth:	dialogRoot.width
		implicitHeight:	saveButton.height + (buttonRow.padding * 2)

		Row
		{
			id:					buttonRow
			spacing:			jaspTheme.rowSpacing
			padding:			jaspTheme.generalAnchorMargin
			anchors.centerIn:	parent

			RectangularButton
			{
				id:			saveButton
				text:		qsTr("Save")
				width:		discardButton.width
				onClicked:
				{
					dialogRoot.save();
					dialogRoot.close();
				}
			}

			RectangularButton
			{
				id:			cancelButton
				text:		qsTr("Cancel")
				width:		discardButton.width
				onClicked:
				{
					dialogRoot.cancel();
					dialogRoot.close();
				}
			}

			RectangularButton
			{
				id:			discardButton
				text:		qsTr("Discard")
				onClicked:
				{
					dialogRoot.discard();
					dialogRoot.close();
				}
			}
		}
	}
}
