import QtQuick			2.11
import QtQuick.Controls	2.4
import JASP.Widgets		1.0
import JASP.Controls	1.0
import JASP.Theme		1.0
import QtQuick.Layouts  1.3
import "." as SEM

JASPControl
{
    id:					    factorsForm
	controlType:		    "FactorsForm"
    implicitWidth:	        parent.width
	height:			        Theme.defaultVariablesFormHeight + Math.max((factorsFormRepeater.count - 3), 0) * (factorsForm.factorListHeight + factorsFormColumn.spacing)
    implicitHeight:         height
    useControlMouseArea:    false

	property var	model
	property string availableVariablesListName: "allAvailableVariables"
	property alias	availableVariablesList: availableVariablesList
    property bool   allowAll: false
	property int	initNumberFactors: 1
	property int    listWidth:			parent.width * 2 / 5
    property int    factorListHeight: (Theme.defaultVariablesFormHeight - factorButtons.height) / 3 - factorsFormColumn.spacing 

	signal titleChanged(int index, string title);
	signal factorAdded(int index, var item);

    VariablesList
	{
		id:				availableVariablesList
		name:			availableVariablesListName
		width:			listWidth
		height:			parent.height
		anchors.top:	parent.top
		anchors.left:	parent.left
    }
    
    Column 
    {
        id:             factorsFormColumn
        spacing:        10 
        width:          parent.width * 3 / 5
		anchors.top:	parent.top
		anchors.right:	parent.right
        
        Repeater 
        {
            id: factorsFormRepeater
			model: factorsForm.model
			RowLayout
            {
				property alias factorList: factorList
				property alias button: button
                spacing: 0
				AssignButton
                {
					id: button
					name: "Factor form "
                    Layout.leftMargin:  (factorsFormColumn.width / 3 - width) / 2
                    Layout.rightMargin: (factorsFormColumn.width / 3 - width) / 2
					leftSource:         factorsForm.availableVariablesList
					rightSource:        factorList

					Component.onDestruction:
					{
						availableVariablesList.activeFocusChanged.disconnect(button.setIconToRight);
						availableVariablesList.hasSelectedItemsChanged.disconnect(button.setState);
					}

                }
                SEM.FactorsList 
                {
					id:					factorList
					name:               factorName
					editableTitle:      factorTitle
					dropMode:			"Replace"
					suggestedColumns:	allowAll ? [] : ["scale"]
                    allowedColumns:     ["scale"]
                    implicitWidth:      listWidth
					height:             factorsForm.factorListHeight

					onTitleIsChanged: factorsForm.titleChanged(index, editableTitle)
				}
			}
			onItemAdded:
			{
				item.factorList.activeFocusChanged.connect(item.button.setIconToLeft);
				availableVariablesList.activeFocusChanged.connect(item.button.setIconToRight);
				item.factorList.hasSelectedItemsChanged.connect(item.button.setState);
				availableVariablesList.hasSelectedItemsChanged.connect(item.button.setState);
				factorsForm.factorAdded(index, item.factorList);
			}
        }

        Row 
        {
            id:             factorButtons
            anchors.right:  parent.right
            spacing:        10

            Button 
            { 
                name: "add"; 
                text: qsTr("+")
                control.width: height 
                width: control.width
				onClicked: factorsForm.model.addFactor()
            }
            Button 
            { 
                name: "remove"; 
                text: qsTr("-") 
                control.width: height 
                width: control.width
				onClicked: factorsForm.model.removeFactor()
                enabled: factorsFormRepeater.count > 1
            }
        }
        
	}
}
