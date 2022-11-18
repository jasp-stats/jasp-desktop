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


		onActiveFocusChanged: { currentIndex = count - 2; }
		Keys.onPressed: (event) =>
			{
				event.accepted = true;
				if (event.key === Qt.Key_Backtab || event.key === Qt.Key_Left)
				{
					if (currentIndex === 0)
						datalibrarylist.selectLast();
					else
						decrementCurrentIndex();
				}
				if (event.key === Qt.Key_Tab || event.key === Qt.Key_Right)
				{
					if (currentIndex === count - 2)
						datalibrarylist.selectFirst();
					else
						incrementCurrentIndex();
				}

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
		tabbingEscapes:			true

		anchors
		{
			top:				secondseparator.bottom
			bottom:				parent.bottom
			left:				menuHeader.left
			right:				menuHeader.right
			topMargin:			jaspTheme.generalMenuMargin
			bottomMargin:		jaspTheme.generalMenuMargin
		}

		KeyNavigation.tab:		datalibrarybreadcrumbs
		KeyNavigation.backtab:	datalibrarybreadcrumbs
	}
}
