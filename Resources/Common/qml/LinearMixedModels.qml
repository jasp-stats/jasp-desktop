//
// Copyright (C) 2013-2019 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

import QtQuick 2.8
import QtQuick.Layouts 1.3
import JASP.Controls 1.0
import JASP.Widgets 1.0
import JASP.Theme 1.0


Form
{
	VariablesForm
	{
		AssignedVariablesList
		{
			name: "dependent"
			title: qsTr("Dependent Variable")
			singleItem: true
			allowedColumns: ["scale"]
		}

		AssignedVariablesList
		{
			name: "fixedFactors"
			title: qsTr("Fixed Factors")
			allowedColumns: ["ordinal", "nominal"]
		}

		AssignedVariablesList
		{
			name: "fixedCovariates"
			title: qsTr("Fixed Covariates")
			allowedColumns: ["scale"]
		}

		AssignedVariablesList
		{
			name: "randomFactors"
			title: qsTr("Random Effects Grouping Factors")
			allowedColumns: ["ordinal", "nominal"]
		}
	}

	ComboBox
	{
		name: "estimation"
		currentIndex: 2
		text: qsTr("Estimation method")

		model: ListModel
		{
			ListElement { key: "A"; value: "type1"}
			ListElement { key: "B"; value: "type2"}
			ListElement { key: "C"; value: "type3"}
			ListElement { key: "D"; value: "type4"}
		}
	}


	ExpanderButton
	{
		text: qsTr("Fixed Effects")

		VariablesForm
		{
			height: 200
			availableVariablesList
			{
				title: qsTr("Components")
				name: "components"
				source: ["fixedFactors", "fixedCovariates"]
			}
			AssignedVariablesList
			{
				title: qsTr("Model terms")
				name: "modelTerms"
				listViewType: "Interaction"
			}
		}

		CheckBox { text: qsTr("Test intercept"); name: "testIntercept" }
	}

	ExpanderButton
	{
		text: qsTr("Random Effects")

		// TODO: Widget similar to repeated measures anova

		VariablesForm
		{
			height: 200
			availableVariablesList
			{
				name: "randomEffectsAvailable"
				source: ["fixedFactors", "fixedCovariates"]
			}
			AssignedVariablesList
			{
				name: "randomEffectsChosen"
				listViewType: "Interaction"
			}
		}
	}

	ExpanderButton
	{
		text: qsTr("Post Hoc Tests")

		VariablesForm
		{
			height: 200
			availableVariablesList
			{
				name: "postHocTestsAvailable"
				source: "fixedFactors"
			}
			AssignedVariablesList
			{
				name: "postHocTestsVariables"
			}
		}

		Group
		{
			title: qsTr("Correction")
			CheckBox { text: qsTr("No correction"); name: "postHocNoCorrection"   ; checked: true }
			CheckBox { text: qsTr("Bonferroni")   ; name: "postHocTestsBonferroni"				}
			CheckBox { text: qsTr("Holm")		 ; name: "postHocTestsHolm"					  }
		}
	}

	ExpanderButton
	{
		text: qsTr("Descriptives Plots")

		VariablesForm
		{
			height: 170
			availableVariablesList {		title: qsTr("Factors")		  ; name: "descriptivePlotsVariables" ; source: ["fixedFactors", "randomFactors"] }
			AssignedVariablesList {  title: qsTr("Horizontal axis")  ; name: "plotHorizontalAxis"	; singleItem: true }
			AssignedVariablesList {		 title: qsTr("Separate lines")   ; name: "plotSeparateLines"	 ; singleItem: true }
			AssignedVariablesList {		 title: qsTr("Separate plots")   ; name: "plotSeparatePlots"	 ; singleItem: true }
		}
	}

	ExpanderButton
	{
		text: qsTr("Assumption Checks")

		Group
		{
			CheckBox { text: qsTr("Homogeneity tests"); name: "homogeneityTests" }
			CheckBox
			{
				text: qsTr("Homogeneity corrections"); name: "homogeneityCorrections"
				columns: 3
				CheckBox { text: qsTr("None")		   ; name: "homogeneityNone" ; checked: true }
				CheckBox { text: qsTr("Brown-Forsythe") ; name: "homogeneityBrown" ; checked: true }
				CheckBox { text: qsTr("Welch")		  ; name: "homogeneityWelch" ; checked: true }
			}
			CheckBox { text: qsTr("Q-Q plot of residuals") ; name: "qqPlot" }
		}
	}

	ExpanderButton
	{
		text: qsTr("Contrasts")

		ContrastsList
		{
			source: ["fixedFactors", "randomFactors"]
		}

		CheckBox { text: qsTr("Assume equal variances") ; name: "contrastAssumeEqualVariance" ; checked: true }
		CheckBox
		{
			text: qsTr("Confidence intervals"); name: "confidenceIntervalsContrast"
			childrenOnSameRow: true
			PercentField { name: "confidenceIntervalIntervalContrast"; defaultValue: 95 }
		}
	}

	ExpanderButton
	{
		text: qsTr("Additional Options")
		columns: 1

		Label { text: qsTr("Marginal means") }
		VariablesForm
		{
			height: 200
			availableVariablesList { name: "marginalMeansTermsAvailable" ; source: "modelTerms"; showVariableTypeIcon: false }
			AssignedVariablesList {  name: "marginalMeansTerms"; showVariableTypeIcon: false }
		}

		CheckBox
		{
			text: qsTr("Compare marginal means to 0")	; name: "marginalMeansCompareMainEffects"
			ComboBox
			{
				name: "marginalMeansCIAdjustment";
				text: qsTr("Confidence interval adjustment");

				model: ListModel
				{
					ListElement {key: "None"; value: "none"}
					ListElement {key: "Bonferro"; value: "bonferroni"}
					ListElement {key: "Sidak"; value: "sidak"}
				}
			}
		}

		Group
		{
			title: qsTr("Display")
			CheckBox { text: qsTr("Descriptive statistics")	 ; name: "descriptives" }
			CheckBox
			{
				text: qsTr("Estimates of effect size")   ; name: "effectSizeEstimates"
				columns: 3
				CheckBox { text: qsTr("η²")		 ; name: "effectSizeEtaSquared"; checked: true }
				CheckBox { text: qsTr("partial η²") ; name: "effectSizePartialEtaSquared" }
				CheckBox { text: qsTr("ω²")		 ; name: "effectSizeOmegaSquared" }
			}
			CheckBox { text: qsTr("Vovk-Sellke maximum p-ratio"); name: "VovkSellkeMPR" }
		}
	}

	ExpanderButton
	{
		text: qsTr("Simple Main Effects")

		VariablesForm
		{
			height: 170
			availableVariablesList {		title: qsTr("Factors")			  ; name: "effectsVariables"	  ; source:  ["fixedFactors", "randomFactors"] }
			AssignedVariablesList {  title: qsTr("Simple effect factor") ; name: "simpleFactor"		  ; singleItem: true }
			AssignedVariablesList {		 title: qsTr("Moderator factor 1")   ; name: "moderatorFactorOne"	; singleItem: true }
			AssignedVariablesList {		 title: qsTr("Moderator factor 2")   ; name: "moderatorFactorTwo"	; singleItem: true }
		}
	}

	ExpanderButton
	{
		text: qsTr("Non parametrics")

		VariablesForm
		{
			height: 200
			availableVariablesList {		title: qsTr("Kruskal-Wallis test")  ; name: "kruskalVariablesAvailable";  source:  ["fixedFactors", "randomFactors"] }
			AssignedVariablesList {  name: "kruskalVariablesAssigned" }
		}
	}

}
