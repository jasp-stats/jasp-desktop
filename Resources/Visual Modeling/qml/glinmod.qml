import QtQuick			2.12
import JASP.Controls	1.0
import JASP.Widgets		1.0
import JASP				1.0

Form 
{
	usesJaspResults: true

	VariablesForm
	{
		AvailableVariablesList	{ name: "allVariables" }
		AssignedVariablesList	{ name: "dependent"	; title: qsTr("Dependent Variable")			; singleVariable: true	}
		AssignedVariablesList	{ name: "variables"	; title: qsTr("Independent Variable(s)")	; id: vars				}
		DropDown
		{
			id: family
			name: "family"
			values: ["Normal", "Logistic", "Poisson", "Negative binomial", "Gamma"]
			label: qsTr("Distribution family")
		}
	}


	Section
	{
		title: qsTr("Interaction terms")
		enabled: vars.count > 1
		VariablesForm
		{
			height: 150
			AvailableVariablesList	{ name: "components"	; title: qsTr("Components")		; source: ["variables"]				}
			AssignedVariablesList	{ name: "interactions"	; title: qsTr("Model terms")	; listViewType: JASP.Interaction	}
		}
	}

	/* ExpanderButton{
	title: qsTr("Family and Link Function")



		  DropDown{
			  name: "link"
			  values: ["identity", "logit", "log", "inverse", "custom..."]
			  {
				if (["Normal", "Logistic"].includes(family.currentText)) return ["identity", "logit", "log", "inverse", "custom..."]
				else if (["Poisson", "Gamma"].includes(family.currentText)) return ["inverse", "custom..."]
				else return ["identity", "test"]
			   }
			  label: qsTr("Link function")
		  }
  }  */


	Section
	{
		title: qsTr("Results Displays")

		Group
		{
			title: qsTr("Plots")
			CheckBox { name:"model"			; label: qsTr("Model plot")	; checked: true	}
			CheckBox { name:"univariates"	; label: qsTr("Univariates")				}
		}


		Group
		{
			title: qsTr("Estimation")
			CheckBox { name:"ests"			; label: qsTr("Show parameter Estimates"); checked: true }
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
				value: 0
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
				name:"ghost";
				label: qsTr("Ghost lines");
				checked: true
				enabled: vars.count > 1 & vars.count< 4
			}
		}

	}
}
