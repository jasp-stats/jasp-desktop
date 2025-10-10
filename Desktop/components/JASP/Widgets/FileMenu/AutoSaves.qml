import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import JASP.Controls
import JASP.Widgets

Item
{
	id:						rect
	focus:					true
	onActiveFocusChanged:	if(activeFocus)
							{
								autoSavesList.forceActiveFocus()

								if(fileMenuModel.autoSaves.listModel)
									fileMenuModel.autoSaves.listModel.refresh();
							}

	MenuHeader
	{
		id:					menuHeader
		headertext:			qsTr("Recovery")
	}


	ScrollMoreIndicator
	{
		anchors
		{
			top:			autoSavesList.top
			left:			autoSavesList.left
			right:			autoSavesList.right
		}

		upsideDown:	true
		extraSpace:	autoSavesList.contentY
	}

	ScrollMoreIndicator
	{
		anchors
		{
			left:			 autoSavesList.left
			right:			 autoSavesList.right
			bottom:			 autoSavesList.bottom
		}

		upsideDown:	false
		extraSpace:	autoSavesList.contentHeight - (autoSavesList.contentY + autoSavesList.height)
	}

	FileList
	{
		id:					autoSavesList
		cppModel:			fileMenuModel.autoSaves.listModel
		tabbingEscapes:		true


		anchors
		{
			top:			menuHeader.bottom
			left:			menuHeader.left
			right:			menuHeader.right
			bottom:			deletionWarning.top
			topMargin:		jaspTheme.generalMenuMargin
			bottomMargin:	jaspTheme.generalMenuMargin
		}
	}

	Label
	{
		text:				qsTr("No recovery files found")
		font:				jaspTheme.font
		color:				jaspTheme.black
		visible:			autoSavesList.count == 0
		anchors.top:		autoSavesList.top
		anchors.left:		autoSavesList.left
	}

	Label
	{
		id:					deletionWarning
		text:				"<i>" + qsTr("Recovery files are deleted after a month") + "</i>"
		font:				jaspTheme.font
		color:				jaspTheme.black
		anchors.bottom:		parent.bottom
		anchors.margins:	jaspTheme.generalMenuMargin
		anchors.left:		parent.left
	}

}
