import QtQuick
import JASP.Controls
import JASP
import QtQuick.Layouts

/*!
    \qmltype FactorsForm
    \inqmlmodule JASP.Controls 1.0
    \brief A form for defining latent factors by assigning observed variables.

    Combines an AvailableVariablesList with dynamically created FactorsList panels.
    Users can add or remove factors and assign variables to each using assign buttons.
    Commonly used in Exploratory/Confirmatory Factor Analysis.

    \section1 R Binding

    \list
    \li \b{R Type:} list (one entry per factor containing name, title, and assigned variable names)
    \li \b{Default:} One empty factor
    \endlist

    \section1 Properties

    \list
    \li \b name (string) - R option name this control binds to. Default: "".
    \li \b availableVariablesListName (string) - Name of the available variables source. Default: "allAvailableVariables".
    \li \b allowedColumns (array) - Column types allowed for assignment. Default: ["scale"].
    \li \b initNumberFactors (int) - Initial number of factor lists shown. Default: 1.
    \li \b baseName (string) - Base name for factor R option keys (not translated). Default: "Factor".
    \li \b baseTitle (string) - Base display title for factors (translated). Default: "Factor".
    \li \b startIndex (int) - Starting index for factor numbering. Default: 1.
    \li \b allowInteraction (bool) - Allow interaction terms in factor lists. Default: false.
    \li \b allowTypeChange (bool) - Allow changing variable type icons. Default: false.
    \li \b addInteractionsByDefault (bool) - Automatically add interactions. Default: false.
    \li \b nested (bool) - Use nested factor structure. Default: false.
    \endlist

    \section1 Inherited Properties

    \list
    \li \b enabled (bool) - Whether the control is interactive. Default: true.
    \li \b visible (bool) - Whether the control is visible. Default: true.
    \li \b info (string) - Info that will be used by tooltip and to generate the help. Default: "".
    \li \b toolTip (string) - This property overwrite info property, in order to display a simpler tooltip text. Default: "".
    \endlist

    \section1 Example

    \qml
    FactorsForm {
        name: "factors"
        initNumberFactors: 2
        allowedColumns: ["scale"]
    }
    \endqml
*/
FactorsFormBase
{
	id:					   factorsForm

	implicitHeight		: jaspTheme.defaultVariablesFormHeight + Math.max((factorsFormRepeater.count - 3), 0) * (factorsForm.factorListHeight + factorsFormColumn.spacing)
	implicitWidth		: jaspForm.width
	height				: implicitHeight
	width				: implicitWidth
	Layout.columnSpan	: parent.columns
	optionKey			: allowInteraction ? "components" : "indicators" // The option has the name, the title and the terms of each VariablesList: optionKey gives the key name for the terms.


	property string availableVariablesListName:		"allAvailableVariables"
	property alias	availableVariablesList:			availableVariablesList
	property var	allowedColumns:					["scale"]
	property int    listWidth:						parent.width * 2 / 5
	property int    factorListHeight:				(jaspTheme.defaultVariablesFormHeight - factorButtons.height) / 3 - factorsFormColumn.spacing
	property int	assignAvailableVariablesToList:	allowInteraction ? (initNumberFactors - 1) : -1 // If interaction is used, set automatically the available variables to the last assigned variables list
	property bool	allowTypeChange:				false
	property bool	addInteractionsByDefault:		false
	property alias	keepAvailableVariables:			availableVariablesList.keepVariablesWhenMoved

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
					}

                }
				FactorsList
                {
					id:					factorList
					name:               factorName
					editableTitle:      factorTitle
					dropKeys:			availableVariablesListName
					//dropMode:			JASP.DropReplace
					allowedColumns:     factorsForm.allowedColumns
					allowTypeChange:	factorsForm.allowTypeChange
					implicitHeight:		factorsForm.factorListHeight // preferredHeight does not work when changing the language: the height is set to the implicitHeight
					implicitWidth:		listWidth
					isBound:			false
					listViewType:		allowInteraction ? JASP.Interaction : JASP.AssignedVariables
					addInteractionsByDefault: factorsForm.addInteractionsByDefault
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
