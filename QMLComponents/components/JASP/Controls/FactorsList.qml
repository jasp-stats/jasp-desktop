import QtQuick			2.8
import JASP.Controls	1.0

AssignedVariablesList
{
	property alias editableTitle: titleField.value
	signal titleIsChanged()
	title: " " //dummy

	TextField
    {
		id: titleField
		isBound: false
        anchors.top: parent.top
        anchors.left: parent.left
        fieldWidth: parent.width
    }

	Component.onCompleted: titleField.editingFinished.connect(titleIsChanged);

}
