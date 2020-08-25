 import QtQuick			2.12
import JASP.Controls	1.0
import JASP.Widgets		1.0
import JASP				1.0

Form 
{
	VariablesForm
	{
		AvailableVariablesList	{ name: "allVariables" }
		AssignedVariablesList	{ name: "dependent"	; title: qsTr("Dependent Variable")		; singleVariable: true	; allowedColumns: ["scale"]	}
		AssignedVariablesList	{ name: "variables"	; title: qsTr("Independent Variable(s)"); id: varlist											}
	}

	Section
	{
		title: qsTr("Model Terms");
		enabled: varlist.count > 0 ;

		VariablesForm
		{
			height: 150
			AvailableVariablesList	{ name: "components"	; title: qsTr("Components")	; source: ["variables"] }
			AssignedVariablesList	{ name: "interactions"	; id: "interactions" ; title: qsTr("Model terms"); listViewType: JASP.Interaction
				rowComponentsTitles: ["Add as a polynomial"]
				rowComponents: [ Component { CheckBox { name: "polynoms" } } ]
			}
		}
	}

	Section
	{
		title: qsTr("Visual Fitting")
		DropDown
		{
			name: "linetype"
			values: ["Regression", "Quadratic", "Cubic"]
			label: qsTr("Fitted line (scatterplots)")
			enabled: varlist.count > 0
		}


	}

	Section
	{
		title: qsTr("Results Displays");


		Group
		{
			title: qsTr("Plots")
			CheckBox { name:"model"			; label: qsTr("Model plot")	; checked: true							}
			CheckBox { name:"univariate"	; label: qsTr("Univariate")											}
			CheckBox { name:"residuals"		; label: qsTr("Diagnostics")										}
			CheckBox { name:"avp"			; label: qsTr("Added variable plot")	; enabled: interactions.count > 1	}
		}


		Group
		{
			title: qsTr("Estimation")
			CheckBox { name:"modinf"		; label: qsTr("Show model comparisons"); checked:true }
			CheckBox { name:"means"			; label: qsTr("Report means")			; checked: true				}
			CheckBox { name:"diff"			; label: qsTr("Show mean differences")	; checked: true				}
			CheckBox { name:"sl"			; label: qsTr("Show slopes/intercepts")	; checked: true				}
			CheckBox { name:"ci"			; label: qsTr("Show 95% intervals")		; checked: true				}
			CheckBox { name:"pval"			; label: qsTr("Show p-values")		; checked: false				}
		}
	}

	Section
	{
		title: qsTr("Plot Controls")

		Group
		{
			title: qsTr("Point controls")
			columns: 4
			Slider
			{
				name: "alpha"
				label: qsTr("Point transparency")
				value: 0.4
				vertical: true
				enabled: varlist.count > 0
			}
			Slider
			{
				name: "jitx"
				label: qsTr("Jitter in X")
				value: .1
				min: 0
				max: .5
				vertical: true
				enabled: varlist.count > 0
			}
			Slider
			{
				name: "jity"
				label: qsTr("Jitter in Y")
				value: 0
				min: 0
				max: .5
				vertical: true
				enabled: varlist.count > 0
			}
		}
		Group
		{
			title: qsTr("Aesthetics")
			DropDown
			{
				name: "theme"
				values: ["JASP", "Black and white", "Minimal", "Classic", "Dark"]
				label: qsTr("GGplot theme")
			}
			CheckBox
			{
				name:"bw";
				label: qsTr("Convert to grayscale");
				checked: false
			}
			CheckBox
			{
				name:"ghost";
				label: qsTr("Ghost lines");
				checked: true
				enabled: varlist.count > 1 & varlist.count< 4
			}
		}
	}
}
