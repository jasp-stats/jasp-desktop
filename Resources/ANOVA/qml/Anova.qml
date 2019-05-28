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
		AvailableVariablesList { name: "allVariablesList" }		
		AssignedVariablesList { name: "dependent";		title: qsTr("Dependent Variable");	suggestedColumns: ["scale"]; singleVariable: true		}
		AssignedVariablesList { name: "fixedFactors";	title: qsTr("Fixed Factors");		suggestedColumns: ["ordinal", "nominal"]				}
		AssignedVariablesList { name: "randomFactors";	title: qsTr("Random Factors");		suggestedColumns: ["ordinal", "nominal"];	debug: true }
		AssignedVariablesList { name: "wlsWeights";		title: qsTr("WLS Weights");			suggestedColumns: ["scale"]; singleVariable: true		}
	}
	
	Section
	{
		title: qsTr("Model")
		
		VariablesForm
		{
			height: 200
			AvailableVariablesList { name: "components"; title: qsTr("Components"); source: ["fixedFactors", "randomFactors"] }
			AssignedVariablesList {  name: "modelTerms"; title: qsTr("Model Terms"); listViewType: "Interaction" }
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
		
	}
	
	Section
	{
		title: qsTr("Assumption Checks")
		
		Group
		{
			CheckBox { name: "homogeneityTests";	label: qsTr("Homogeneity tests")			}
			CheckBox
			{
				name: "homogeneityCorrections";		label: qsTr("Homogeneity corrections")
				columns: 3
				CheckBox { name: "homogeneityNone";		label: qsTr("None")           ; checked: true }
				CheckBox { name: "homogeneityBrown";	label: qsTr("Brown-Forsythe") ; checked: true }
				CheckBox { name: "homogeneityWelch";	label: qsTr("Welch")          ; checked: true }
			}
			CheckBox { name: "qqPlot"; label: qsTr("Q-Q plot of residuals") }
		}
	}
	
	Section
	{
		title: qsTr("Contrasts")
		
		ContrastsList { source: ["fixedFactors", "randomFactors"] }
		
		CheckBox { name: "contrastAssumeEqualVariance"; label: qsTr("Assume equal variances"); checked: true }
		CheckBox
		{
			name: "confidenceIntervalsContrast"; label: qsTr("Confidence intervals")
			childrenOnSameRow: true
			CIField {	name: "confidenceIntervalIntervalContrast" }
		}
	}
	
	Section
	{
		title: qsTr("Post Hoc Tests")
		
		VariablesForm
		{
			height: 200
			AvailableVariablesList { name: "postHocTestsAvailable"; source: "modelTerms" }
			AssignedVariablesList {  name: "postHocTestsVariables" }

		}

        Group
        {
            CheckBox
            {
                name: "confidenceIntervalsPostHoc"; label: qsTr("Confidence intervals")
                childrenOnSameRow: true
                CIField {name: "confidenceIntervalIntervalPostHoc" }
            }
            CheckBox
            {
                name: "postHocTestsBootstrapping"; label: qsTr("From")
                childrenOnSameRow: true
                IntegerField
                {
                    name: "postHocTestsBootstrappingReplicates"
                    defaultValue: 1000
                    fieldWidth: 50
                    min: 100
                    afterLabel: qsTr("bootstraps")
                }
            }
        }

        CheckBox { name: "postHocTestEffectSize";	label: qsTr("Effect Size") }

		Group
		{
			title: qsTr("Correction")
			CheckBox { name: "postHocTestsTukey";		label: qsTr("Tukey"); checked: true	}
			CheckBox { name: "postHocTestsScheffe";		label: qsTr("Scheffe")				}
			CheckBox { name: "postHocTestsBonferroni";	label: qsTr("Bonferroni")			}
			CheckBox { name: "postHocTestsHolm";		label: qsTr("Holm")					}
            CheckBox { name: "postHocTestsSidak";       label: qsTr("Šidák")                }
		}

		Group
		{
			title: qsTr("Type")
			CheckBox { name: "postHocTestsTypeStandard";	label: qsTr("Standard"); checked: true	}
			CheckBox { name: "postHocTestsTypeGames";		label: qsTr("Games-Howell")				}
			CheckBox { name: "postHocTestsTypeDunnett";		label: qsTr("Dunnett")					}
			CheckBox { name: "postHocTestsTypeDunn";		label: qsTr("Dunn")						}
		}
	}
	
	Section
	{
		title: qsTr("Descriptives Plots")
		
		VariablesForm
		{
			height: 150
			AvailableVariablesList { name: "descriptivePlotsVariables"; title: qsTr("Factors"); source: ["fixedFactors", "randomFactors"] }
			AssignedVariablesList { name: "plotHorizontalAxis";			title: qsTr("Horizontal Axis"); singleVariable: true }
			AssignedVariablesList { name: "plotSeparateLines";			title: qsTr("Separate Lines");	singleVariable: true }
			AssignedVariablesList { name: "plotSeparatePlots";			title: qsTr("Separate Plots");	singleVariable: true }
		}
		
		Group
		{
			title: qsTr("Display")
			CheckBox
			{
				name: "plotErrorBars"; label: qsTr("Display error bars")
				RadioButtonGroup
				{
					name: "errorBarType"
					RadioButton
					{
						value: "confidenceInterval";		label: qsTr("Confidence interval"); checked: true
						childrenOnSameRow: true
						CIField { name: "confidenceIntervalInterval" }
					}
					RadioButton { value: "standardError";	label: qsTr("Standard error") }
				}
			}
		}
	}
	
	Section
	{
		title: qsTr("Additional Options")
		columns: 1
		
		VariablesForm
		{
			height: 200
			AvailableVariablesList { name: "marginalMeansTermsAvailable"; title: qsTr("Marginal Means"); source: "modelTerms" }
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
					{ label: "None",		value: "none"},
					{ label: "Bonferroni",	value: "bonferroni"},
					{ label: "Šidák",		value: "sidak"}
				]
			}
		}
		
		Group
		{
			title: qsTr("Display")
			CheckBox { name: "descriptives";	label: qsTr("Descriptive statistics")	}
			CheckBox {
				name: "effectSizeEstimates";	label: qsTr("Estimates of effect size")
				columns: 3
				CheckBox { name: "effectSizeEtaSquared";		label: qsTr("η²"); checked: true	}
				CheckBox { name: "effectSizePartialEtaSquared";	label: qsTr("partial η²")		}
				CheckBox { name: "effectSizeOmegaSquared";		label: qsTr("ω²")				}
			}
			CheckBox { name: "VovkSellkeMPR"; label: qsTr("Vovk-Sellke maximum p-ratio") }
		}
	}
	
	Section
	{
		title: qsTr("Simple Main Effects")
		
		VariablesForm
		{
			height: 170
			AvailableVariablesList { name: "effectsVariables";	title: qsTr("Factors")				; source:  ["fixedFactors", "randomFactors"] }
			AssignedVariablesList {	name: "simpleFactor";		title: qsTr("Simple Effect Factor") ; singleVariable: true }
			AssignedVariablesList { name: "moderatorFactorOne";	title: qsTr("Moderator Factor 1")	; singleVariable: true }
			AssignedVariablesList { name: "moderatorFactorTwo";	title: qsTr("Moderator Factor 2")	; singleVariable: true }
		}
	}
	
	Section
	{
		title: qsTr("Nonparametrics")
		
		VariablesForm
		{
			height: 170
			AvailableVariablesList { name: "kruskalVariablesAvailable"; title: qsTr("Kruskal-Wallis Test"); source:  ["fixedFactors", "randomFactors"] }
			AssignedVariablesList {  name: "kruskalVariablesAssigned" }
		}
	}	
}
