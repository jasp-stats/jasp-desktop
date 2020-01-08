import QtQuick			2.8
import JASP.Controls	1.0 as JC
import JASP				1.0

JC.VariablesList
{
	property alias editableTitle: titleField.value
	signal titleIsChanged()
	title: " " //dummy
	listViewType: JASP.AssignedVariables

    JC.TextField 
    {
		id: titleField
		isBound: false
        anchors.top: parent.top
        anchors.left: parent.left
        preferredHeight: text.height
        fieldWidth: parent.width
    }

	Component.onCompleted: titleField.editingFinished.connect(titleIsChanged);

}
