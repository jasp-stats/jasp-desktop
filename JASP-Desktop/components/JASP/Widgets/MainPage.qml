import QtQuick 2.11
import QtWebEngine 1.7
import QtQuick.Controls 2.4
import QtQuick.Controls 1.4 as OLD
import QtQuick.Layouts 1.3
import JASP.Theme 1.0
import JASP.Widgets 1.0

OLD.SplitView
{
	orientation:	Qt.Horizontal

	DataPanel
	{
		id:						data
		width:					visible ? 300 : 0
		Layout.fillWidth:		visible
		Layout.minimumWidth:	visible ? 200 : 0
		visible:				true
	}

	WebEngineView
	{
		url:					"qrc:///core/index.html"
		Layout.minimumWidth:	100
		width:					600
	}
}
