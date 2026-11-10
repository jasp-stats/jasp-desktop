import QtQuick
import JASP.Controls
import JASP
import QtQuick.Layouts
import QtQuick.Controls as QtControls

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
    \li \b nested (bool) - Use nested factor structure. Default: false. Not applied while the term specification is active.
    \li \b advancedAvailable (bool) - Offer the term specification. Default: false (the option, its layout and saved files stay exactly as before). A "Specify" button in the corner of the available list opens a table, full width, where the terms of all models are defined once: "(Intercept)" (see interceptMode), then a base term for every variable (plain, or treatment coded), whose specification can be changed but which cannot be removed; + adds a row for another term (choose its variable, then its transformation (continuous) or coding (categorical)), or a constant offset (a number), which x removes; "Reset" switches the specification off and clears the defined terms; "Back" returns to the lists. The available list then offers these terms by name ("x", "log(x)", "f (reference b)", "offset(2)", ...) and "(Intercept)"; they are assigned to the models as usual (interactions with shift-click), and the models are not forced to be nested. The intercept and offsets are never part of an interaction (the form removes such terms). An icon in the corner of each model's list shows that model full width, with its terms in full (each removable). The table and a model shown full width keep the height of the lists (their rows scroll), so the page does not move. The option then carries per model the optionKey member (`components`), `intercept`, `definitions` and `terms`; read it with jaspBase::prepareModelTerms(). It must not change at runtime, and `availableVariablesList.source` must be a plain value (the form replaces it while the specification is on and puts it back).
    \li \b advanced (bool) - Whether the term specification is on (with advancedAvailable). Set it to true in QML to start new analyses with it: every initial model with "(Intercept)", and a variable added later goes into the model that takes new variables. A saved analysis keeps what it had. Default: false.
    \li \b restrictionAvailable (bool) - Add a "Restriction" column (fixed coefficients) to the term specification. Default: false.
    \li \b interceptMode (string) - "optional": "(Intercept)" is a term, assigned to every model when the specification starts and removable from each; "forced": every model has an intercept; "none": no model has one (e.g. Cox regression). Default: "optional".
    \li \b continuousSpecifications (array) - Specifications offered for continuous variables, in this order. The specifications are listed in jaspBase's inst/modelTermSpecifications.json; "default" stands for every one it marks as default, also the ones a later version adds. The others need support of the analysis and are listed by name: "offset" (the variable with its coefficient fixed at 1, offset(x)), "offsetConstant" (an added row can be a Constant offset, a number), "rank" and "normalScores" (they depend on the whole sample, so the model cannot predict new data), "arrhenius". Unknown names, and names of the other list, are ignored (and logged). Default: ["default"].
    \li \b categoricalSpecifications (array) - Codings offered for categorical (nominal and ordinal) variables, as continuousSpecifications; "indicator" (one column per level, for models without intercept) is not a default. Default: ["default"].
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

	implicitHeight		: listsHeight	// also while the specification table or a model full width replaces the lists: the page does not move
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
	readonly property int listsHeight:				jaspTheme.defaultVariablesFormHeight + Math.max((factorsFormRepeater.count - 3), 0) * (factorsForm.factorListHeight + factorsFormColumn.spacing)

	// Term specification (advancedAvailable): while the terms are being specified, the table replaces the lists
	property bool	editing:						false
	property int	expandedModel:					-1					// a model shown full width with its terms
	property alias	variablesUniverse:				variablesUniverse	// read by FactorsFormBase: the variables the terms are made of
	property bool	_showDefinitions:				false				// the available list offers the defined terms
	property var	_ownSource							// meanwhile its own source, for the variables and to restore
	property bool	_autoAssign:					true				// the last model takes new available variables (basic mode only)

	AvailableVariablesList
	{
		id:				availableVariablesList
		name:			availableVariablesListName
		visible:		!factorsForm.editing && factorsForm.expandedModel < 0
		width:			listWidth
		preferredHeight: listsHeight
		anchors.top:	parent.top
		anchors.left:	parent.left
    }

	AvailableVariablesList // the analysis' variables, also while the available list offers the defined terms
	{
		id:				variablesUniverse
		name:			factorsForm.advancedAvailable ? factorsForm.name + "TermVariables" : "" // the form sets up named controls only (in the order of their sources)
		visible:		false
		width:			0
		preferredHeight: 0
		source:			factorsForm._showDefinitions ? factorsForm._ownSource : factorsForm.advancedAvailable ? availableVariablesList.source : [{ values: [] }]
	}

	Binding // only with the term specification: its models are independent, a term may go into several;
	{		// otherwise what an interaction (or plain) assigned list sets for its available list
		when:		factorsForm.advancedAvailable
		target:		availableVariablesList
		property:	"keepVariablesWhenMoved"
		value:		factorsForm.advancedActive || factorsForm.allowInteraction
		restoreMode: Binding.RestoreNone
	}

	Button // in the bottom-right corner of the available list, muted until hovered
	{
		id:				specifyButton
		visible:		factorsForm.advancedAvailable && !factorsForm.editing && factorsForm.expandedModel < 0
		text:			qsTr("Specify")
		z:				availableVariablesList.z + 1
		opacity:		control.hovered ? 1 : 0.55
		anchors.bottom:	availableVariablesList.bottom
		anchors.right:	availableVariablesList.right
		anchors.margins: 6 * jaspTheme.uiScale
		anchors.rightMargin: 6 * jaspTheme.uiScale + (availableVariablesList.scrollBar.visible ? availableVariablesList.scrollBar.width : 0) // left of the scroll bar
		info:			factorsForm.advancedAvailable ? qsTr("Specify the terms of the models: transform or code each variable, define a variable several times with different specifications, and fix coefficients where offered. The terms are then assigned to the models like variables, interactions included, and the models need not be nested.") : "" // no help entry where there is no button
		onClicked:
		{
			factorsForm.advanced	= true;
			factorsForm.editing		= factorsForm.advancedActive;
		}
	}

	FactorsFormModelView
	{
		id:				modelView
		factorsForm:	factorsForm
		factorIndex:	factorsForm.expandedModel
		visible:		factorsForm.expandedModel >= 0
		width:			parent.width
		height:			factorsForm.listsHeight
		anchors.top:	parent.top
		onBack:			factorsForm.expandedModel = -1
	}

	FactorsFormDefinitions
	{
		id:				definitionsView
		factorsForm:	factorsForm
		visible:		factorsForm.editing
		width:			parent.width
		height:			factorsForm.listsHeight
		anchors.top:	parent.top
		onBack:			factorsForm.editing = false
	}

	Connections
	{
		target: factorsForm
		// FactorsFormBase has already given the models their new terms: now the available list follows (the defined terms,
		// or the variables again), without adding them to the last model as new variables
		function onAdvancedActiveChanged() { factorsForm._followAdvanced(); }
	}

	// also once the form is complete: a form that starts with the term specification (advanced: true) switched it on
	// before this Connections listened
	Component.onCompleted: _followAdvanced()

	function _followAdvanced() // the available list follows the specification: the defined terms, or the variables again
	{
		if (factorsForm.advancedActive && !factorsForm._showDefinitions)
		{
			factorsForm._autoAssign					= false;
			factorsForm._ownSource					= availableVariablesList.source;
			factorsForm._showDefinitions			= true;	// the variables list keeps that source
			availableVariablesList.source			= Qt.binding(function() { return [{ values: factorsForm.definitionTerms }]; });
		}
		else if (!factorsForm.advancedActive && factorsForm._showDefinitions)
		{
			factorsForm.editing						= false;
			availableVariablesList.source			= factorsForm._ownSource;
			factorsForm._showDefinitions			= false;
			Qt.callLater(function() { factorsForm._autoAssign = !factorsForm.advancedActive; });
		}
	}
    
    Column 
    {
        id:             factorsFormColumn
        visible:        !factorsForm.editing && factorsForm.expandedModel < 0
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
					addAvailableVariablesToAssigned: index === assignAvailableVariablesToList && factorsForm._autoAssign

					onTitleIsChanged:	factorsForm.titleChanged(index, editableTitle)
				}

				Item // the expand icon in the top-right corner of the model's list: the model full width with its terms
				{
					parent:					factorList
					visible:				factorsForm.advancedAvailable
					z:						10
					width:					12 * jaspTheme.uiScale
					height:					width
					anchors.top:			parent.top
					anchors.right:			parent.right
					anchors.topMargin:		3 * jaspTheme.uiScale
					anchors.rightMargin:	4 * jaspTheme.uiScale
					opacity:				expandMouseArea.containsMouse ? 1 : 0.55

					QtControls.ToolTip.text:				qsTr("Show this model's terms")
					QtControls.ToolTip.visible:				expandMouseArea.containsMouse
					QtControls.ToolTip.toolTip.background:	Rectangle { color: jaspTheme.tooltipBackgroundColor; radius: jaspTheme.borderRadius }

					Canvas // four corners
					{
						anchors.fill:	parent

						property color strokeColor:	jaspTheme.textEnabled
						onStrokeColorChanged:		requestPaint()	// another theme

						onPaint:
						{
							var c = getContext("2d"), w = width, h = height, k = Math.round(w * 0.38);
							c.reset();
							c.strokeStyle	= strokeColor;
							c.lineWidth		= Math.max(1.2, 1.4 * jaspTheme.uiScale);
							c.beginPath();
							c.moveTo(1, k);			c.lineTo(1, 1);			c.lineTo(k, 1);
							c.moveTo(w - k, 1);		c.lineTo(w - 1, 1);		c.lineTo(w - 1, k);
							c.moveTo(w - 1, h - k);	c.lineTo(w - 1, h - 1);	c.lineTo(w - k, h - 1);
							c.moveTo(k, h - 1);		c.lineTo(1, h - 1);		c.lineTo(1, h - k);
							c.stroke();
						}
					}

					MouseArea
					{
						id:				expandMouseArea
						anchors.fill:	parent
						hoverEnabled:	true
						cursorShape:	Qt.PointingHandCursor
						onClicked:		{ factorsForm.editing = false; factorsForm.expandedModel = index; }
					}
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
