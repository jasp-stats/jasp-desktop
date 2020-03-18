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

import QtQuick			2.12
import JASP.Controls	1.0
import JASP.Widgets		1.0
import JASP				1.0

Form
{	
	IntegerField { visible: false; name: "plotHeightDescriptivesPlotLegend"     ; defaultValue: 300 }
	IntegerField { visible: false; name: "plotHeightDescriptivesPlotNoLegend"   ; defaultValue: 300 }
	IntegerField { visible: false; name: "plotWidthDescriptivesPlotLegend"      ; defaultValue: 450 }
	IntegerField { visible: false; name: "plotWidthDescriptivesPlotNoLegend"    ; defaultValue: 350 }
	
	
	VariablesForm
	{
		preferredHeight: 520 * preferencesModel.uiScale
		AvailableVariablesList		{ name: "allVariablesList" }
		RepeatedMeasuresFactorsList { name: "repeatedMeasuresFactors"; title: qsTr("Repeated Measures Factors"); height: 180 }
		AssignedRepeatedMeasuresCells
		{
			name:				"repeatedMeasuresCells"
			title:				qsTr("Repeated Measures Cells")
			source:				"repeatedMeasuresFactors"
		}
		AssignedVariablesList
		{
			name:				"betweenSubjectFactors"
			title:				qsTr("Between Subject Factors")
			suggestedColumns:	["ordinal", "nominal"]
			itemType:			"fixedFactors"
		}
		AssignedVariablesList
		{
			name:				"covariates"
			title:				qsTr("Covariates")
			suggestedColumns:	["scale"]
		}
	}
	
	Group
	{
		title: qsTr("Display")
		CheckBox { name: "descriptives";		label: qsTr("Descriptive statistics") }
		CheckBox
		{
			name: "effectSizeEstimates";	label: qsTr("Estimates of effect size")
			columns: 3
			CheckBox { name: "effectSizeEtaSquared";		label: qsTr("η²")         ; checked: true	}
			CheckBox { name: "effectSizePartialEtaSquared";	label: qsTr("partial η²")					}
			CheckBox { name: "effectSizeGenEtaSquared";	label: qsTr("general η²")					}
			CheckBox { name: "effectSizeOmegaSquared";		label: qsTr("ω²")							}
		}
		CheckBox { name: "VovkSellkeMPR";					label: qsTr("Vovk-Sellke maximum p-ratio")	}
	}
	
	Section
	{
		title: qsTr("Model")
		
		VariablesForm
		{
			preferredHeight: 150 * preferencesModel.uiScale
			AvailableVariablesList	{ name: "withinComponents"; title: qsTr("Repeated Measures Components"); source: ["repeatedMeasuresFactors"] }
			AssignedVariablesList	{ name: "withinModelTerms"; title: qsTr("Model Terms");	listViewType: JASP.Interaction	}
		}
		
		VariablesForm
		{
			preferredHeight: 150 * preferencesModel.uiScale
			AvailableVariablesList	{ name: "betweenComponents"; title: qsTr("Between Subjects Components"); source: ["betweenSubjectFactors", "covariates"] }
			AssignedVariablesList	{ name: "betweenModelTerms"; title: qsTr("Model terms"); listViewType: JASP.Interaction }
		}
		
		DropDown
		{
			name: "sumOfSquares"
			indexDefaultValue: 2
			label: qsTr("Sum of squares")
			values: [
				{ label: "Type \u2160", value: "type1"},
				{ label: "Type \u2161", value: "type2"},
				{ label: "Type \u2162", value: "type3"}
			]
		}
		CheckBox { name: "useMultivariateModelFollowup";				label: qsTr("Use multivariate model for follow-up tests");					checked: false }

		
	}
	
	Section
	{
		title: qsTr("Assumption Checks")

		Group
		{
			CheckBox { name: "sphericityTests";	label: qsTr("Sphericity tests") }
			Group
			{
				title: qsTr("Sphericity corrections")
				columns: 3
				CheckBox { name: "sphericityNone";				label: qsTr("None");					checked: true }
				CheckBox { name: "sphericityGreenhouseGeisser";	label: qsTr("Greenhouse-Geisser");	checked: false }
				CheckBox { name: "sphericityHuynhFeldt";		label: qsTr("Huynh-Feldt");			checked: false }
			}
			CheckBox { name: "homogeneityTests"; label: qsTr("Homogeneity tests") }
		}
	}
	
	Section
	{
		title: qsTr("Contrasts")
		ContrastsList { source: ["repeatedMeasuresFactors", "betweenSubjectFactors"] }
		
		CheckBox { name: "contrastAssumeEqualVariance"; label: qsTr("Assume equal variances"); checked: true }
		CheckBox
		{
			name: "confidenceIntervalsContrast"; label: qsTr("Confidence intervals")
			childrenOnSameRow: true
			CIField { name: "confidenceIntervalIntervalContrast" }
		}
	}
	
	Section
	{
		title: qsTr("Post Hoc Tests")
		columns: 1

		VariablesForm
		{
			preferredHeight: 150 * preferencesModel.uiScale
			AvailableVariablesList { name: "postHocTestsAvailable"; source: ["withinModelTerms", { name: "betweenModelTerms", discard: "covariates" }]; mixedModelTerms: true }
			AssignedVariablesList {  name: "postHocTestsVariables" }
		}
		
		CheckBox
		{
			name: "confidenceIntervalsPostHoc"; label: qsTr("Confidence intervals")
			childrenOnSameRow: true
			CIField {name: "confidenceIntervalIntervalPostHoc" }
		}

		Group
		{
			columns: 2
			CheckBox { name: "postHocTestEffectSize";	label: qsTr("Effect size")						}
			CheckBox { name: "postHocTestPooledError";	label: qsTr("Pool error term for RM factors");			checked: true	}
		}
		
		Group
		{
			title: qsTr("Correction")
			CheckBox { name: "postHocTestsHolm";		label: qsTr("Holm"); 		checked: true	}
			CheckBox { name: "postHocTestsBonferroni";	label: qsTr("Bonferroni")			}
			CheckBox { name: "postHocTestsTukey";		label: qsTr("Tukey")				}
			CheckBox { name: "postHocTestsScheffe";		label: qsTr("Scheffe")				}
		}
		
		Group
		{
			title: qsTr("Display")
			CheckBox { name: "postHocFlagSignificant";	label: qsTr("Flag Significant Comparisons") }
		}
	}
	
	Section
	{
		title: qsTr("Descriptives Plots")
		columns: 1
		
		VariablesForm
		{
			preferredHeight: 150 * preferencesModel.uiScale
			AvailableVariablesList { name: "descriptivePlotsVariables"; title: qsTr("Factors");			source: ["repeatedMeasuresFactors", "betweenSubjectFactors"] }
			AssignedVariablesList {  name: "plotHorizontalAxis";		title: qsTr("Horizontal Axis"); singleVariable: true }
			AssignedVariablesList {  name: "plotSeparateLines";			title: qsTr("Separate Lines");	singleVariable: true }
			AssignedVariablesList {  name: "plotSeparatePlots";			title: qsTr("Separate Plots");	singleVariable: true }
		}
		
		TextField { name: "labelYAxis"; label: qsTr("Label y-axis"); fieldWidth: 200 }
		Group
		{
			title: qsTr("Display")
			columns: 2
			CheckBox
			{
				name: "plotErrorBars"; label: qsTr("Display error bars")
				RadioButtonGroup
				{
					name: "errorBarType"
					RadioButton
					{
						value: "confidenceInterval"; label: qsTr("Confidence interval"); checked: true
						childrenOnSameRow: true
						CIField { name: "confidenceIntervalInterval" }
					}
					RadioButton { value: "standardError"; label: qsTr("Standard error") }
				}
			}
			CheckBox { name: "usePooledStandErrorCI";	label: qsTr("Average across unused RM factors")	}
			
		}
	}
	
	Section
	{
		title: qsTr("Marginal Means")
		columns: 1

		Group
		{
			title: qsTr("Marginal Means")
			
			VariablesForm
			{
				preferredHeight: 150 * preferencesModel.uiScale
				AvailableVariablesList { name: "marginalMeansTermsAvailable" ; source: ["withinModelTerms", { name: "betweenModelTerms", discard: "covariates" }] }
				AssignedVariablesList {  name: "marginalMeansTerms" }
			}
			
			CheckBox
			{
				name: "marginalMeansBootstrapping"; label: qsTr("From")
				childrenOnSameRow: true
				IntegerField
				{
					name: "marginalMeansBootstrappingReplicates"
					defaultValue: 1000
					fieldWidth: 50
					min: 100
					afterLabel: qsTr("bootstraps")
				}
			}

			CheckBox
			{
				name: "marginalMeansCompareMainEffects"; label: qsTr("Compare marginal means to 0")
				DropDown
				{
					name: "marginalMeansCIAdjustment"
					label: qsTr("Confidence interval adjustment")
					values: [
						{ label: qsTr("None"),		value: "none"},
						{ label: qsTr("Bonferroni"),	value: "bonferroni"},
						{ label: qsTr("Šidák"),		value: "sidak"}
					]
				}
			}
		}
		
	}
	
	Section
	{
		title: qsTr("Simple Main Effects")
		
		VariablesForm
		{
			preferredHeight: 150 * preferencesModel.uiScale
			AvailableVariablesList { name: "effectsVariables";	title: qsTr("Factors"); source: ["repeatedMeasuresFactors", "betweenSubjectFactors"] }
			AssignedVariablesList {  name: "simpleFactor";		title: qsTr("Simple Effect Factor");	singleVariable: true }
			AssignedVariablesList { name: "moderatorFactorOne";	title: qsTr("Moderator Factor 1");		singleVariable: true }
			AssignedVariablesList { name: "moderatorFactorTwo";	title: qsTr("Moderator Factor 2");		singleVariable: true }
		}
		
		CheckBox { name: "poolErrorTermSimpleEffects"; label: qsTr("Pool error terms") }
	}
	
	Section
	{
		title: qsTr("Nonparametrics")
		
		VariablesForm
		{
			preferredHeight: 150 * preferencesModel.uiScale
			AvailableVariablesList { name: "kruskalVariablesAvailable"; title: qsTr("Factors"); source: ["repeatedMeasuresFactors", "betweenSubjectFactors"] }
			AssignedVariablesList {  name: "friedmanWithinFactor";		title: qsTr("RM Factor") }
			AssignedVariablesList {  name: "friedmanBetweenFactor";		title: qsTr("Optional Grouping Factor"); singleVariable: true }
		}
		
		CheckBox { name: "conoverTest"; label: qsTr("Conover's post hoc tests") }
	}
}
