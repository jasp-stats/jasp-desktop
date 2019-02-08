//
// Copyright (C) 2013-2018 University of Amsterdam
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
import JASP.Controls 1.0
import JASP.Widgets 1.0

Form
{
	usesJaspResults: false
	
	IntegerField { visible: false; name: "plotWidthQQPlot"                      ; defaultValue: 300 }
	IntegerField { visible: false; name: "plotHeightQQPlot"                     ; defaultValue: 300 }
	IntegerField { visible: false; name: "plotHeightDescriptivesPlotLegend"     ; defaultValue: 300 }
	IntegerField { visible: false; name: "plotHeightDescriptivesPlotNoLegend"   ; defaultValue: 300 }
	IntegerField { visible: false; name: "plotWidthDescriptivesPlotLegend"      ; defaultValue: 430 }
	IntegerField { visible: false; name: "plotWidthDescriptivesPlotNoLegend"    ; defaultValue: 350 }
	
	VariablesForm
	{
		height: 400
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
			name: "randomFactors"
			title: qsTr("Random Factors")
			allowedColumns: ["ordinal", "nominal"]
			debug: true
		}
		AssignedVariablesList
		{
			name: "covariates"
			title: qsTr("Covariates")
			allowedColumns: ["scale"]
		}
		AssignedVariablesList
		{
			name: "wlsWeights"
			title: qsTr("WLS Weights")
			singleItem: true
			allowedColumns: ["scale"]
		}
	}
	
	
	ExpanderButton
	{
		title: qsTr("Model")
		
		VariablesForm
		{
			height: 200
			availableVariablesList { name: "components"; title: qsTr("Components"); source: ["fixedFactors", "randomFactors", "covariates"] }
			AssignedVariablesList  { name: "modelTerms"; title: qsTr("Model terms"); listViewType: "Interaction" }
		}
		
		DropDown
		{
			name: "sumOfSquares"
			indexDefaultValue: 2
			text: qsTr("Sum of squares")
			model: ListModel
			{
				ListElement { title: "Type \u2160"; value: "type1" }
				ListElement { title: "Type \u2161"; value: "type2" }
				ListElement { title: "Type \u2162"; value: "type3" }
			}
		}
		
	}
	
	ExpanderButton
	{
		title: qsTr("Assumption Checks")
		
		Group
		{
			CheckBox { name: "homogeneityTests";			text: qsTr("Homogeneity tests")									}
			CheckBox { name: "qqPlot";						text: qsTr("Q-Q plot of residuals")								}
			CheckBox { name: "factorCovariateIndependence";	text: qsTr("Factor covariate independence check"); debug: true	}
		}
	}
	
	ExpanderButton
	{
		title: qsTr("Contrasts")
		
		ContrastsList {}
		
		CheckBox { name: "contrastAssumeEqualVariance"; text: qsTr("Assume equal variances"); checked: true }
		CheckBox
		{
			name: "confidenceIntervalsContrast"; text: qsTr("Confidence intervals")
			childrenOnSameRow: true
			PercentField { name: "confidenceIntervalIntervalContrast"; defaultValue: 95 }
		}
	}
	
	ExpanderButton
	{
		title: qsTr("Post Hoc Tests")
		
		VariablesForm
		{
			height: 200
			availableVariablesList { name: "postHocTestsAvailable"; source: "fixedFactors" }
			AssignedVariablesList {  name: "postHocTestsVariables" }
		}
		
		CheckBox { name: "postHocTestEffectSize";	text: qsTr("Effect Size") }

		CheckBox
		{
			name: "confidenceIntervalsPostHoc"; text: qsTr("Confidence intervals")
			childrenOnSameRow: true
			PercentField {name: "confidenceIntervalIntervalPostHoc"; defaultValue: 95 }
		}
		
		Group
		{
			title: qsTr("Correction")
			CheckBox { name: "postHocTestsTukey";		text: qsTr("Tukey"); checked: true	}
			CheckBox { name: "postHocTestsScheffe";		text: qsTr("Scheffe")				}
			CheckBox { name: "postHocTestsBonferroni";	text: qsTr("Bonferroni")			}
			CheckBox { name: "postHocTestsHolm";		text: qsTr("Holm")					}
		}

		Group
		{
			title: qsTr("Type")
			CheckBox { name: "postHocTestsTypeStandard";	text: qsTr("Standard"); checked: true	}
			CheckBox { name: "postHocTestsTypeGames";		text: qsTr("Games-Howell")				}
			CheckBox { name: "postHocTestsTypeDunnett";		text: qsTr("Dunnett")					}
			CheckBox { name: "postHocTestsTypeDunn";		text: qsTr("Dunn")						}
		}
	}
	
	ExpanderButton
	{
		title: qsTr("Descriptives Plots")
		
		VariablesForm {
			height: 200
			availableVariablesList { name: "descriptivePlotsVariables"; title: qsTr("Factors"); source: "fixedFactors"	}
			AssignedVariablesList {	name: "plotHorizontalAxis";			title: qsTr("Horizontal axis"); singleItem: true	}
			AssignedVariablesList {	name: "plotSeparateLines";			title: qsTr("Separate lines"); singleItem: true		}
			AssignedVariablesList { name: "plotSeparatePlots";			title: qsTr("Separate plots"); singleItem: true		}
		}
		
		Group
		{
			title: qsTr("Display")
			CheckBox
			{
				name: "plotErrorBars"; text: qsTr("Display error bars")
				RadioButtonGroup
				{
					name: "errorBarType"
					RadioButton
					{
						value: "confidenceInterval"; text: qsTr("Confidence Interval"); checked: true
						childrenOnSameRow: true
						PercentField { name: "confidenceIntervalInterval";	text: qsTr("Interval"); defaultValue: 95 }
					}
					RadioButton { value: "standardError"; text: qsTr("Standard error") }
				}
			}
		}
	}
	
	ExpanderButton
	{
		title: qsTr("Additional Options")
		columns: 1
		
		Label { text: qsTr("Marginal means") }
		VariablesForm
		{
			height: 200
			availableVariablesList { name: "marginalMeansTermsAvailable" ; source: "modelTerms"; showVariableTypeIcon: false }
			AssignedVariablesList {	 name: "marginalMeansTerms"; showVariableTypeIcon: false }
		}
		
		CheckBox
		{
			name: "marginalMeansCompareMainEffects"; text: qsTr("Compare marginal means to 0")
			DropDown
			{
				name: "marginalMeansCIAdjustment"
				text: qsTr("Confidence interval adjustment")
				model: ListModel
				{
					ListElement { title: "None";		value: "none"		}
					ListElement { title: "Bonferro";	value: "bonferroni"	}
					ListElement { title: "Sidak";		value: "sidak"		}
				}
			}
		}
		
		Group
		{
			title: qsTr("Display")
			CheckBox { name: "descriptives"; text: qsTr("Descriptive statistics") }
			CheckBox
			{
				name: "effectSizeEstimates"; text: qsTr("Estimates of effect size")
				columns: 3
				CheckBox { name: "effectSizeEtaSquared";		text: qsTr("η²"); checked: true	}
				CheckBox { name: "effectSizePartialEtaSquared";	text: qsTr("partial η²")		}
				CheckBox { name: "effectSizeOmegaSquared";		text: qsTr("ω²")				}
			}
			CheckBox { name: "VovkSellkeMPR"; text: qsTr("Vovk-Sellke maximum p-ratio") }
		}
	}
	
	ExpanderButton
	{
		title: qsTr("Simple Main Effects")
		
		VariablesForm
		{
			height: 160
			availableVariablesList { name: "effectsVariables";	title: qsTr("Factors");	source: "fixedFactors" }
			AssignedVariablesList {	name: "simpleFactor";		title: qsTr("Simple effect factor"); singleItem: true }
			AssignedVariablesList {	name: "moderatorFactorOne";	title: qsTr("Moderator factor 1"); singleItem: true }
			AssignedVariablesList {	name: "moderatorFactorTwo";	title: qsTr("Moderator factor 2"); singleItem: true }
		}
	}
	
	ExpanderButton
	{
		title: qsTr("Nonparametrics")
		columns: 1
		
		Group
		{
			Label { text: qsTr("Kruskal-Wallis test") }
			VariablesForm
			{
				height: 200
				availableVariablesList { name: "kruskalVariablesAvailable"; source: "fixedFactors" }
				AssignedVariablesList {	name: "kruskalVariablesAssigned" }
			}
		}
		
		CheckBox { name: "dunnTest"; text: qsTr("Dunn's post hoc test") }
	}
	
}
