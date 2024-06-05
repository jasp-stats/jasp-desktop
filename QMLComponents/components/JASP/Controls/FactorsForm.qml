import QtQuick			2.11
import QtQuick.Controls	2.4
import JASP.Controls	1.0
import JASP				1.0
import QtQuick.Layouts  1.3

FactorsFormBase
{
    id:					    factorsForm

	implicitHeight		: jaspTheme.defaultVariablesFormHeight + Math.max((factorsFormRepeater.count - 3), 0) * (factorsForm.factorListHeight + factorsFormColumn.spacing)
	implicitWidth		: jaspForm.width
	height				: implicitHeight
	width				: implicitWidth
	Layout.columnSpan	: parent.columns
	optionKey			: allowInteraction ? "components" : "indicators" // The option has the name, the title and the terms of each VariablesList: optionKey gives the key name for the terms.


	property string availableVariablesListName: "allAvailableVariables"
	property alias	availableVariablesList: availableVariablesList
    property bool   allowAll: false
	property int    listWidth:			parent.width * 2 / 5
	property int    factorListHeight: (jaspTheme.defaultVariablesFormHeight - factorButtons.height) / 3 - factorsFormColumn.spacing
	property int	assignAvailableVariablesToList:	allowInteraction ? (initNumberFactors - 1) : -1 // If interaction is used, set automatically the available variables to the last assigned variables list

	AvailableVariablesList
	{
		id:				availableVariablesList
		name:			availableVariablesListName
		width:			listWidth
		preferredHeight: parent.height
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
				property alias	factorList		: factorList
				property alias	button			: button
				property bool	isDynamic		: true

                spacing: 0
				AssignButton
                {
					id: button
					name: "Factor form"
                    Layout.leftMargin:  (factorsFormColumn.width / 3 - width) / 2
                    Layout.rightMargin: (factorsFormColumn.width / 3 - width) / 2
					leftSource:         factorsForm.availableVariablesList
					rightSource:        factorList

					Component.onDestruction:
					{
						availableVariablesList.activeFocusChanged.disconnect(button.setIconToRight);
						availableVariablesList.selectedItemsChanged.disconnect(button.setState);
					}

                }
				FactorsList
                {
					id:					factorList
					name:               factorName
					editableTitle:      factorTitle
					dropKeys:			availableVariablesListName
					//dropMode:			JASP.DropReplace
					suggestedColumns:	allowAll ? [] : ["scale", "ordinal"]
					allowedColumns:     allowAll ? [] : ["scale", "ordinal"]
					implicitHeight:		factorsForm.factorListHeight // preferredHeight does not work when changing the language: the height is set to the implicitHeight
					implicitWidth:		listWidth
					isBound:			false
					listViewType:		allowInteraction ? JASP.Interaction : JASP.AssignedVariables
					addAvailableVariablesToAssigned: index === assignAvailableVariablesToList

					onTitleIsChanged:	factorsForm.titleChanged(index, editableTitle)
				}
			}
			onItemAdded: (index, item) =>
			{
				availableVariablesList.dropKeys.push(item.factorList.name);
				for (let i = 0; i < index; i++)
				{
					itemAt(i).factorList.dropKeys.push(item.factorList.name)
					item.factorList.dropKeys.push(itemAt(i).factorList.name)
				}

				item.factorList.activeFocusChanged.connect(item.button.setIconToLeft);
				availableVariablesList.activeFocusChanged.connect(item.button.setIconToRight);
				item.factorList.selectedItemsChanged.connect(item.button.setState);
				availableVariablesList.selectedItemsChanged.connect(item.button.setState);
				factorsForm.factorAdded(index, item.factorList);
			}
        }

        Row 
        {
			id:						factorButtons
			anchors.right:			parent.right
			anchors.rightMargin:	(listWidth - copyButton.width * 2 - spacing) / 2
			spacing:				2

			MenuButton
			{
				id:					copyButton
				width:				height
				iconSource:			jaspTheme.iconPath + "/round_addition.png"
				onClicked:			factorsForm.addFactor()
				toolTip:			qsTr("Add a %1").arg(baseTitle)
				radius:				height
			}

			MenuButton
			{
				id:					closeButton
				width:				height
				iconSource:			jaspTheme.iconPath + "close-button.png"
				opacity:			enabled ? 1 : .5
				enabled:			factorsFormRepeater.count > initNumberFactors
				onClicked:			factorsForm.removeFactor()
				toolTip:			qsTr("Remove last %1").arg(baseTitle)
				radius:				height
			}
        }
        
	}
}
