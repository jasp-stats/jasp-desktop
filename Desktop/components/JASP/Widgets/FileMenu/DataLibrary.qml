import QtQuick 2.9
import QtQuick.Controls 2.2
import QtQuick.Layouts 1.3


Item
{
	id:							rect
	focus:						true
	onActiveFocusChanged:		if(activeFocus)
								{
									fileMenuModel.datalibrary.listModel.resetPath();
									datalibrarylist.forceActiveFocus();
								}

	MenuHeader
	{
		id:						menuHeader
		headertext:				qsTr("Data Library")
		toolseparator:			false
	}

	BreadCrumbs
	{
		id:						datalibrarybreadcrumbs
		model:					fileMenuModel.datalibrary.breadcrumbsmodel
		onCrumbButtonClicked:	(modelIndex)=>{ model.indexChanged(modelIndex) }

		anchors
		{
			top:				menuHeader.bottom
			left:				parent.left
			right:				parent.right
			leftMargin:			jaspTheme.generalMenuMargin
			rightMargin:		jaspTheme.generalMenuMargin
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
		keyNavigationWraps:		true

		anchors
		{
			top:				secondseparator.bottom
			bottom:				parent.bottom
			left:				menuHeader.left
			right:				menuHeader.right
			topMargin:			jaspTheme.generalMenuMargin
			bottomMargin:		jaspTheme.generalMenuMargin
		}
	}
}
