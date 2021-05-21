import QtQuick			2.15
import QtQuick.Controls 2.15
import JASP.Controls	1.0

Popup
{
	id:			popupRenameColumnDialog;
	modal:		true;

	y:			(parent.height / 2) - (height / 2)
	x:			(parent.width / 2)  - (width / 2)
	width:		popupLoader.width
	height:		popupLoader.height+1

	property int colIndex;

	background: Rectangle
	{
		color:			jaspTheme.uiBackground
		border.color:	jaspTheme.uiBorder
		border.width:	1
	}
	padding:	0

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

	Loader
	{
		id:					popupLoader
		sourceComponent:	visible ? renameComp : null
		visible:			popupRenameColumnDialog.opened
	}

	Component
	{
		id:	renameComp

		Item
		{
			height:	renameButton.y + renameButton.height + jaspTheme.generalAnchorMargin
			width:	200 * jaspTheme.uiScale

			Component.onCompleted:	columnName.forceActiveFocus();

			Text
			{
				id:					title
				text:				qsTr("Rename column")
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


			TextInput
			{
				id:						columnName
				text:					dataSetModel.columnName(popupRenameColumnDialog.colIndex)
				color:					jaspTheme.textEnabled
				font:					jaspTheme.fontGroupTitle
				selectByMouse:			true
				anchors
				{
					top:				title.bottom
					left:				parent.left
					right:				parent.right
					margins:			jaspTheme.generalAnchorMargin
				}

				onEditingFinished:		renameButton.clicked();
				Keys.onEnterPressed:	renameButton.clicked();

				KeyNavigation.tab:		renameButton
				KeyNavigation.down:		renameButton

				Rectangle
				{
					color:				jaspTheme.controlBackgroundColor
					border.width:		1
					border.color:		jaspTheme.borderColor
					radius:				jaspTheme.borderRadius
					z:					-1
					anchors.fill:		parent
					anchors.margins:	-jaspTheme.jaspControlPadding
				}

				MouseArea
				{
					acceptedButtons:	Qt.NoButton
					anchors.fill:		parent
					cursorShape:		Qt.IBeamCursor
				}
			}

			RectangularButton
			{
				id:						renameButton
				activeFocusOnTab:		true
				text:					qsTr("Rename")
				onClicked:				{ dataSetModel.setColumnName(popupRenameColumnDialog.colIndex, columnName.text); popupRenameColumnDialog.close(); }
				toolTip:				qsTr("Rename column %1 to %2").arg(popupRenameColumnDialog.colIndex + 1).arg(columnName.text)
				KeyNavigation.right:	closeButtonCross
				KeyNavigation.tab:		closeButtonCross

				anchors
				{
					top:				columnName.bottom
					margins:			jaspTheme.generalAnchorMargin
					topMargin:			jaspTheme.generalAnchorMargin + jaspTheme.jaspControlPadding
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
				height:					renameButton.height
				onClicked:				popupResizeData.close()
				toolTip:				qsTr("Close without renaming column")
				KeyNavigation.up:		columnName

				anchors
				{
					right:				parent.right
					top:				columnName.bottom
					margins:			jaspTheme.generalAnchorMargin
				}
			}
		}
	}
}
