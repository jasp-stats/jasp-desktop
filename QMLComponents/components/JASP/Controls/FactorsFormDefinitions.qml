import QtQuick
import QtQuick.Layouts
import QtQuick.Controls as QtControls
import JASP.Controls
import JASP

/*!
    \qmltype FactorsFormDefinitions
    \inqmlmodule JASP.Controls 1.0
    \brief The "Specify" view of a FactorsForm: the terms all models can use, defined once.

    Internal to FactorsForm (shown full width while its terms are being specified). The first row is "(Intercept)"; each
    other row defines a term: a variable with a specification (a transformation of a continuous variable or a coding of
    a categorical one) or a constant offset (a number), and, where the analysis allows it, a restriction. The
    specifications and the editors of their parameters come from the catalogue in jaspBase. Every variable
    has a base row (its variable fixed, not removable); added rows start empty and can be removed. The state lives in
    the FactorsForm (FactorsFormBase::definitionRows and its setters); this view only shows it, so its controls are not
    bound to options themselves.
*/
Rectangle
{
	id: view

	property var	factorsForm
	property real	variableWidth:			130 * jaspTheme.uiScale
	property real	restrictionWidth:		80 * jaspTheme.uiScale
	property real	specificationWidth:		143 * jaspTheme.uiScale
	property real	parameterWidth:			40 * jaspTheme.uiScale

	// Every committed edit rebuilds the rows; the field that had the focus (e.g. after Tab) gets it back.
	property string focusKey: ""

	readonly property var rows: { factorsForm.specificationsRevision; return factorsForm.advancedActive ? factorsForm.definitionRows() : []; }

	signal back()

	color:			jaspTheme.controlBackgroundColor
	border.width:	1
	border.color:	jaspTheme.borderColor
	radius:			jaspTheme.borderRadius

	// Translations of the titles in the catalogue of specifications (jaspBase inst/modelTermSpecifications.json); a
	// specification, parameter or choice missing here shows the catalogue's English title.
	readonly property var specificationLabels: ({
		none:					qsTr("Not transformed"),
		center:					qsTr("Center: x − c"),
		scale:					qsTr("Scale: (x − mean) / sd"),
		log:					qsTr("Natural log: ln(x)"),
		logShift:				qsTr("Shifted log: ln(x + c)"),
		logBase:				qsTr("General log: c · log_b(x)"),
		reciprocal:				qsTr("Reciprocal: 1/x"),
		power:					qsTr("Power: x^p"),
		root:					qsTr("Root: x^(1/d)"),
		boxCox:					qsTr("Box-Cox: (x^λ − 1) / λ"),
		yeoJohnson:				qsTr("Yeo-Johnson"),
		asinh:					qsTr("Inverse hyperbolic sine: asinh(x)"),
		logit:					qsTr("Logit: ln(x / (1 − x))"),
		probit:					qsTr("Probit: Φ⁻¹(x)"),
		arcsineSqrt:			qsTr("Arcsine square root: asin(√x)"),
		polynomial:				qsTr("Polynomial (orthogonal)"),
		rawPolynomial:			qsTr("Polynomial (raw): x, x², …"),
		naturalSpline:			qsTr("Natural cubic spline"),
		bSpline:				qsTr("B-spline"),
		hinge:					qsTr("Hinge: max(x − k, 0)"),
		threshold:				qsTr("Threshold: x > k"),
		binning:				qsTr("Bins (quantile groups)"),
		fourier:				qsTr("Fourier: sin and cos of 2πx / P"),
		rank:					qsTr("Rank"),
		normalScores:			qsTr("Normal scores"),
		arrhenius:				qsTr("Arrhenius: 1/(k_B T)"),
		offset:					qsTr("Offset (coefficient 1)"),
		treatment:				qsTr("Treatment (dummy)"),
		simple:					qsTr("Simple (vs. reference, centered)"),
		sum:					qsTr("Sum"),
		helmert:				qsTr("Helmert (vs. previous levels)"),
		reverseHelmert:			qsTr("Reverse Helmert (vs. later levels)"),
		successiveDifferences:	qsTr("Successive differences: b − a, c − b, …"),
		forwardDifference:		qsTr("Forward differences: a − b, b − c, …"),
		orthogonalPolynomial:	qsTr("Orthogonal polynomial"),
		sas:					qsTr("SAS"),
		indicator:				qsTr("Indicators (no intercept)")
	})
	readonly property var parameterLabels: ({
		logBase: qsTr("Base"), logMultiplier: qsTr("Multiplier"), exponent: qsTr("Exponent"), rootDegree: qsTr("Degree"),
		polynomialDegree: qsTr("Degree"), splineDf: qsTr("df"), splineDegree: qsTr("Degree"), temperatureUnit: qsTr("From"),
		value: qsTr("Value"), referenceLevel: qsTr("Reference"), shift: qsTr("c"), lambda: qsTr("λ"), center: qsTr("At"),
		knot: qsTr("Knot"), cutoff: qsTr("Cutoff"), bins: qsTr("Bins"), period: qsTr("Period"), harmonics: qsTr("Harmonics")
	})
	readonly property var parameterInfos: ({
		value:	qsTr("The number added to the linear predictor of the models that have this term (its coefficient fixed at 1)."),
		center:	qsTr("mean, median or a number"),
		knot:	qsTr("mean, median or a number"),
		cutoff:	qsTr("mean, median or a number")
	})
	readonly property var choiceLabels: ({ kelvin: qsTr("Kelvin"), celsius: qsTr("Celsius"), fahrenheit: qsTr("Fahrenheit") })

	function specificationValues(row)
	{
		return row.choices.map(function(choice) { return { label: view.specificationLabels[choice.value] || choice.title, value: choice.value }; });
	}

	// The FactorsForm gives the view the height of its lists, so the page does not move: the header and the buttons stay,
	// the rows scroll.
	readonly property real	rowsWidth:	width - 2 * jaspTheme.contentMargin - (scrollBar.visible ? scrollBar.width + 4 * jaspTheme.uiScale : 0)
	property bool			followEnd:	false	// a row was added: show the end of the rows once it is there

	RowLayout
	{
		id:			header
		x:			jaspTheme.contentMargin
		y:			jaspTheme.contentMargin
		width:		view.rowsWidth
		spacing:	8 * jaspTheme.uiScale
		Label { text: qsTr("Variable");			Layout.preferredWidth: view.variableWidth												}
		Label { text: qsTr("Restriction");		Layout.preferredWidth: view.restrictionWidth;	visible: view.factorsForm.restrictionAvailable	}
		Label { text: qsTr("Specification");	Layout.fillWidth: true																	}
	}

	Flickable
	{
		id:					rowsFlickable
		contentHeight:		content.implicitHeight
		clip:				true
		boundsBehavior:		Flickable.StopAtBounds
		anchors
		{
			top:			header.bottom
			topMargin:		4 * jaspTheme.uiScale
			bottom:			buttons.top
			bottomMargin:	4 * jaspTheme.uiScale
			left:			parent.left
			leftMargin:		jaspTheme.contentMargin
			right:			parent.right
			rightMargin:	jaspTheme.contentMargin
		}

		onContentHeightChanged:
		{
			if (view.followEnd)
			{
				view.followEnd	= false;
				contentY		= Math.max(0, contentHeight - height);
			}
			else
				returnToBounds();
		}

		Column
		{
			id:			content
			width:		view.rowsWidth
			spacing:	4 * jaspTheme.uiScale

			Repeater
			{
				model: view.rows

				RowLayout
				{
					id:			rowItem
					required property var modelData
					readonly property var	row:			modelData
					readonly property bool	intercept:		row.kind === "intercept"
					readonly property var	parameters:		intercept ? ({}) : row.parameters
					readonly property string specification:	intercept ? "" : row.specification
					readonly property bool	fixed:			intercept || row.base		// its variable cannot change (the intercept, the base term of a variable)
					readonly property bool	hasVariable:	!intercept && row.variable !== ""
					readonly property bool	constant:		!intercept && row.specification === "offsetConstant"	// a constant offset: a number, no variable

					// committed after the current event: a Tab moves the focus first, so the rebuilt row can give it back
					function set(key, value) { var form = view.factorsForm, id = row.id, text = String(value); Qt.callLater(function() { form.setDefinitionValue(id, key, text); }); } // a new closure each time: callLater merges calls of one function
					function focusOn(key) { view.focusKey = row.id + ":" + key; scrollBar.scrollToElement(rowItem); }
					function hadFocus(key) { return view.focusKey === row.id + ":" + key; }

					width:		content.width
					spacing:	8 * jaspTheme.uiScale
					onYChanged:	if (view.focusKey.indexOf(row.id + ":") === 0) scrollBar.scrollToElement(rowItem)	// a rebuilt row placed after its field got the focus back

					Label
					{
						visible:				rowItem.fixed
						text:					rowItem.intercept ? rowItem.row.label : rowItem.row.variable
						elide:					Text.ElideRight
						Layout.preferredWidth:	view.variableWidth
					}

					DropDown // an added term: its variable is chosen first, which gives it a specification
					{
						visible:			!rowItem.fixed
						isBound:			false
						fieldWidth:			view.variableWidth
						addEmptyValue:		true
						placeholderText:	qsTr("Choose a variable")
						values:				rowItem.fixed ? [] : rowItem.row.variables.map(function(variable) { return { label: variable, value: variable }; })
												.concat(rowItem.row.offsetChoice ? [{ label: qsTr("Constant offset"), value: rowItem.row.offsetChoice }] : []) // when the form offers it
						startValue:			rowItem.fixed ? "" : (rowItem.constant ? rowItem.row.offsetChoice : rowItem.row.variable)
						info:				qsTr("The variable this term is made of, or Constant offset: a number added to the linear predictor (its coefficient fixed at 1).")
						onActivated:		(index) => rowItem.set("variable", index > 0 ? values[index - 1].value : "") // index 0 is the empty entry; `value` is updated only after this handler
					}

					TextField
					{
						visible:			view.factorsForm.restrictionAvailable
						enabled:			rowItem.intercept || (rowItem.hasVariable && rowItem.specification !== "offset") // an offset has no coefficient (a constant offset has no variable)
						isBound:			false
						fieldWidth:			view.restrictionWidth
						value:				rowItem.row.restriction // not its default: clearing the field gives "free"
						placeholderText:	qsTr("free")
						info:				qsTr("Fix the coefficient(s) of this term; leave empty to estimate them. Terms with several coefficients take one value per coefficient, separated by commas.")
						onEditingFinished:	rowItem.set("restriction", displayValue)
						onActiveFocusChanged:	if (activeFocus) rowItem.focusOn("restriction")
						Component.onCompleted:	if (rowItem.hadFocus("restriction")) forceActiveFocus()
					}

					Label
					{
						visible:				rowItem.intercept
						text:					view.factorsForm.interceptMode === "forced" ? qsTr("In every model") : qsTr("A term of every model unless removed from it")
						color:					jaspTheme.textDisabled
						elide:					Text.ElideRight
						Layout.fillWidth:		true
					}

					DropDown
					{
						visible:			rowItem.hasVariable
						isBound:			false
						fieldWidth:			view.specificationWidth
						values:				rowItem.intercept ? [] : view.specificationValues(rowItem.row)
						startValue:			rowItem.specification
						info:				qsTr("How the variable enters the models: transformed (continuous variables) or coded (categorical variables).")
						onActivated:		(index) => rowItem.set("specification", values[index].value)
					}

					Repeater // the parameters of the specification, as the catalogue lists them (FactorsFormBase::definitionRows)
					{
						model: rowItem.intercept ? [] : rowItem.row.parameterInfo

						Loader
						{
							id:							editor
							required property var		modelData
							readonly property var		parameter:	modelData
							readonly property string	current:	rowItem.parameters[parameter.name] || ""
							readonly property string	title:		view.parameterLabels[parameter.name] || parameter.title

							sourceComponent: parameter.type === "integer" ? integerEditor : (parameter.type === "choice" || parameter.type === "level") ? choiceEditor : textEditor

							Component
							{
								id: textEditor	// number, location
								TextField
								{
									isBound:			false
									label:				editor.title
									fieldWidth:			view.parameterWidth * (rowItem.constant ? 2 : editor.parameter.type === "location" ? 1.75 : 1) // a location also takes "median"
									value:				editor.current
									placeholderText:	editor.parameter.placeholder || ""
									info:				view.parameterInfos[editor.parameter.name] || ""
									onEditingFinished:	rowItem.set(editor.parameter.name, displayValue)
									onActiveFocusChanged:	if (activeFocus) rowItem.focusOn(editor.parameter.name)
									Component.onCompleted:	if (rowItem.hadFocus(editor.parameter.name)) forceActiveFocus()
								}
							}
							Component
							{
								id: integerEditor
								IntegerField
								{
									isBound:			false
									label:				editor.title
									min:				editor.parameter.min || 1
									defaultValue:		Number(editor.current || editor.parameter.min || 1)
									onEditingFinished:	rowItem.set(editor.parameter.name, displayValue)
									onActiveFocusChanged:	if (activeFocus) rowItem.focusOn(editor.parameter.name)
									Component.onCompleted:	if (rowItem.hadFocus(editor.parameter.name)) forceActiveFocus()
								}
							}
							Component
							{
								id: choiceEditor	// a choice of the catalogue, or a level of the factor
								DropDown
								{
									isBound:		false
									label:			editor.title
									values:			editor.parameter.type === "level"
														? rowItem.row.levels.map(function(level) { return { label: level, value: level }; })
														: editor.parameter.choices.map(function(choice) { return { label: view.choiceLabels[choice.value] || choice.title, value: choice.value }; })
									startValue:		editor.current
									onActivated:	(index) => rowItem.set(editor.parameter.name, values[index].value)
								}
							}
						}
					}

					Item { visible: !rowItem.intercept; Layout.fillWidth: true; implicitHeight: 1 }

					Image
					{
						source:					jaspTheme.iconPath + "cross.png"
						opacity:				rowItem.row.removable ? 1 : 0	// keeps the column
						Layout.preferredWidth:	jaspTheme.iconSize
						Layout.preferredHeight:	jaspTheme.iconSize

						QtControls.ToolTip.text:				qsTr("Remove this term (from every model)")
						QtControls.ToolTip.visible:				rowItem.row.removable && removeMouseArea.containsMouse
						QtControls.ToolTip.toolTip.background:	Rectangle { color: jaspTheme.tooltipBackgroundColor; radius: jaspTheme.borderRadius }

						MouseArea
						{
							id:				removeMouseArea
							anchors.fill:	parent
							enabled:		rowItem.row.removable
							hoverEnabled:	true
							cursorShape:	Qt.PointingHandCursor
							onClicked:		{ var form = view.factorsForm, id = rowItem.row.id; Qt.callLater(function() { form.removeDefinition(id); }); }
						}
					}
				}
			}
		}
	}

	JASPScrollBar
	{
		id:				scrollBar
		flickable:		rowsFlickable
		manualAnchor:	true
		vertical:		true
		anchors
		{
			top:		rowsFlickable.top
			bottom:		rowsFlickable.bottom
			right:		rowsFlickable.right
		}
	}

	Item
	{
		id:		buttons
		height:	Math.max(addButton.height, backButton.height)
		anchors
		{
			bottom:			parent.bottom
			bottomMargin:	jaspTheme.contentMargin
			left:			parent.left
			leftMargin:		jaspTheme.contentMargin
			right:			parent.right
			rightMargin:	jaspTheme.contentMargin
		}

		MenuButton
		{
			id:					addButton
			width:				height
			radius:				height
			anchors.left:		parent.left
			iconSource:			jaspTheme.iconPath + "round_addition.png"
			toolTip:			qsTr("Add a term: choose its variable, then its specification")
			onClicked:			{ view.followEnd = true; view.factorsForm.addDefinition(); }
		}

		Row
		{
			anchors.right:	parent.right
			spacing:		8 * jaspTheme.uiScale

			Button
			{
				id:			resetButton
				text:		qsTr("Reset")
				opacity:	control.hovered ? 1 : 0.55
				toolTip:	qsTr("Remove the term specification: every model gets the variables of its terms again, and the defined terms are cleared.")
				onClicked:	view.factorsForm.resetDefinitions()
			}

			Button
			{
				id:			backButton
				text:		qsTr("Back")
				toolTip:	qsTr("Back to the models: the available list now offers the terms defined here.")
				onClicked:	view.back()
			}
		}
	}
}
