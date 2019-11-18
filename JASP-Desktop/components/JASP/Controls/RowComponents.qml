import QtQuick 2.0
import QtQuick.Layouts 1.3

RowLayout
{
	id: rowComponents
	anchors.verticalCenter:	parent.verticalCenter
	anchors.right:			parent.right
	anchors.rightMargin:	3 * preferencesModel.uiScale
	spacing:				1
	z:						10
	//layoutDirection:		Qt.RightToLeft

	property var	controls

	onControlsChanged:
	{
		if (controls)
			for (var i = 0; i < controls.length; i++)
				controls[i].parent = rowComponents
	}

}
