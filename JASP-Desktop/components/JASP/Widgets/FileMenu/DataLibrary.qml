import QtQuick 2.9
import QtQuick.Controls 2.2
import QtQuick.Layouts 1.3
import JASP.Theme 1.0

Item
{
	id:							rect
	focus:						true
	onActiveFocusChanged:		if(activeFocus) datalibrarylist.forceActiveFocus()

	MenuHeader
	{
		id:						menuHeader
		headertext:				qsTr("Data Library")
		toolseparator:			false
		anchors
		{
			top:			parent.top
			margins:		Theme.generalMenuMargin
		}
	}

	BreadCrumbs
	{
		id:						datalibrarybreadcrumbs
		model:					fileMenuModel.datalibrary.breadcrumbsmodel
		width:					rect.width
		height:					40 * preferencesModel.uiScale
		onCrumbButtonClicked:	model.indexChanged(modelIndex)

		anchors
		{
			top:				menuHeader.bottom
			left:				parent.left
			right:				parent.right
			leftMargin:			Theme.generalMenuMargin
		}

	}

	ToolSeparator
	{
		id:						secondseparator
		anchors.left:			menuHeader.left
		anchors.right:			menuHeader.right
		anchors.top:			datalibrarybreadcrumbs.bottom
		width:					rect.width
		orientation:			Qt.Horizontal
	}

	FileList
	{
		id:						datalibrarylist
		cppModel:				fileMenuModel.datalibrary.listModel
		breadCrumbs:			datalibrarybreadcrumbs

		anchors
		{
			top:				secondseparator.bottom
			bottom:				parent.bottom
			left:				menuHeader.left
			right:				menuHeader.right
			topMargin:			Theme.generalMenuMargin
			bottomMargin:		Theme.generalMenuMargin
		}
	}
}
