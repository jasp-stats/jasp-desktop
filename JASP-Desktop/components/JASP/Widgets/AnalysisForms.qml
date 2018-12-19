import QtQuick 2.11
import QtQuick.Controls 2.4
import JASP.Widgets 1.0
import JASP.Theme 1.0

ListView
{
	id:								analysesListView

	model:							analysesModel
	highlightFollowsCurrentItem:	true

	delegate: Loader
	{
		source:			formPath
		property bool	currentSelected:	ListView.isCurrentItem
		property Item	listView:			ListView.view
		property int	myIndex:			index
		property string	analysisTitle:		title
	}
}

