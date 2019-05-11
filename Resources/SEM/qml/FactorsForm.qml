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
	height:			        Theme.defaultListHeight
    implicitHeight:         height
    useControlMouseArea:    false

	property var	model
	property string availableVariablesListName: "allAvailableVariables"
	property alias	availableVariablesList: availableVariablesList
    property bool   allowAll: false
	property int	initNumberFactors: 1
	property int    listWidth:			parent.width * 2 / 5
    property int    factorListHeight: (Theme.defaultListHeight - factorButtons.height) / 3 - factorsFormColumn.spacing 

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
                spacing: 0
				AssignButton
                {
					id: button
                    Layout.leftMargin:  (factorsFormColumn.width / 3 - width) / 2
                    Layout.rightMargin: (factorsFormColumn.width / 3 - width) / 2
					leftSource:         factorsForm.availableVariablesList
					rightSource:        factorList
                }
                SEM.FactorsList 
                {
					id:					factorList
					name:               factorName
					editableTitle:      factorTitle
					dropMode:			"Replace"
                    allowedColumns:     allowAll ? [] : ["scale"]
                    implicitWidth:      listWidth
					height:             factorsForm.factorListHeight

					onTitleIsChanged: factorsForm.titleChanged(index, editableTitle)
					Component.onCompleted:
					{
						activeFocusChanged.connect(button.setIconToLeft);
						availableVariablesList.activeFocusChanged.connect(button.setIconToRight);
						hasSelectedItemsChanged.connect(button.setState);
						availableVariablesList.hasSelectedItemsChanged.connect(button.setState);
						factorsForm.factorAdded(index, factorList);
						factorsForm.calculateHeight();
					}
                }
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
                onClicked: addFactor() 
            }
            Button 
            { 
                name: "remove"; 
                text: qsTr("-") 
                control.width: height 
                width: control.width
                onClicked: removeFactor() ; 
                enabled: factorsFormRepeater.count > 1
            }
        }
        
    }

    function addFactor() 
    {
		model.addFactor()
        factorsForm.calculateHeight()
    }

    function removeFactor() 
    {
		model.removeFactor()
        factorsForm.calculateHeight()
    }

    function calculateHeight() 
    {
        if (factorsFormRepeater.count > 3) {
            factorsForm.height = Theme.defaultListHeight + (factorsFormRepeater.count - 3) * (factorsForm.factorListHeight + factorsFormColumn.spacing)
        } else {
            factorsForm.height = Theme.defaultListHeight
        }
    }
    
}
