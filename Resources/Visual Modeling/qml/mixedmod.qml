import QtQuick			2.12
import JASP.Controls	1.0
import JASP.Widgets		1.0
import JASP				1.0

Form 
{
	VariablesForm
	{
		AvailableVariablesList	{ name: "allVariables" }
		AssignedVariablesList	{ name: "dependent"		; title: qsTr("Dependent Variable")			; singleVariable: true	}
		AssignedVariablesList	{ name: "variables"		; title: qsTr("Independent Variable(s)")	; id: varlist			}
		AssignedVariablesList	{ name: "rvariables"	; title: qsTr("Random")						; singleVariable: true	}
	}

	Section
	{
		title: qsTr("Model Builder")
		VariablesForm
		{
			preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
			AvailableVariablesList	{ name: "components"	; title: qsTr("Components")	; source: ["variables"] }
			AssignedVariablesList	{ name: "interactions"	; title: qsTr("Fixed terms"); listViewType: JASP.Interaction
				rowComponentsTitles: ["Add as a random effect"]
				rowComponents: [ Component { CheckBox { name: "randeff2" } } ]
			}
		}
	}

	Section
	{
		title: qsTr("Results Displays")

		Group
		{
			title: qsTr("Plots")
			CheckBox { name:"model"			; label: qsTr("Model plot")			; checked: true }
			CheckBox { name:"univariates"	; label: qsTr("Univariate plots")	; checked: true }
			CheckBox { name:"residuals"		; label: qsTr("Diagnostics")						}

		}
		Group
		{
			title: qsTr("Estimation")
			CheckBox { name:"fixeff"	; label: qsTr("Report fixed effects")	; checked: true		}
			CheckBox { name:"randeff"	; label: qsTr("Report random effects")	; checked: false	}
		}
	}

	Section
	{
		title: qsTr("Plot Controls")

		Group
		{
			title: qsTr("Point controls")
			columns: 4
			Slider { name: "alpha"	; label: qsTr("Point transparency")	; value: 0.4; vertical: true	; enabled: varlist.count > 0					}
			Slider { name: "jitx"	; label: qsTr("Jitter in X")		; value: .1	; vertical: true	; enabled: varlist.count > 0; min: 0; max: .5;	}
			Slider { name: "jity"	; label: qsTr("Jitter in Y")		; value: 0	; vertical: true	; enabled: varlist.count > 0; min: 0; max: .5	}
		}

		Group
		{
			title: qsTr("Other parameters")
			DropDown
			{
				name: "theme"
				values: ["JASP", "Black and white", "Minimal", "Classic", "Dark"]
				label: qsTr("GGplot theme")
			}
			IntegerField
			{
				name: "nsamp"
				label: qsTr("Number of clusters")
				defaultValue: 3
				min: 1
				max: 20
				enabled: varlist.count > 0
			}
		}
	}
}
