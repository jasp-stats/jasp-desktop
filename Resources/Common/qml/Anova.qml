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
import QtQuick.Layouts 1.3
import JASP.Controls 1.0
import JASP.Widgets 1.0
import JASP.Theme 1.0


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
		AssignedVariablesList { name: "dependent";		title: qsTr("Dependent Variable");	allowedColumns: ["scale"]; singleItem: true }
		AssignedVariablesList { name: "fixedFactors";	title: qsTr("Fixed Factors");		allowedColumns: ["ordinal", "nominal"]		}
		AssignedVariablesList { name: "randomFactors";	title: qsTr("Random Factors");		allowedColumns: ["ordinal", "nominal"];	debug: true }
		AssignedVariablesList { name: "wlsWeights";		title: qsTr("WLS Weights");			allowedColumns: ["scale"]; singleItem: true }
	}
	
	
	ExpanderButton
	{
		title: qsTr("Model")
		
		VariablesForm
		{
			height: 200
			availableVariablesList { name: "components"; title: qsTr("Components"); syncModels: ["fixedFactors", "randomFactors"] }
			AssignedVariablesList {  name: "modelTerms"; title: qsTr("Model terms"); listViewType: "Interaction" }
		}
		
		DropDown
		{
			name: "sumOfSquares"
			indexDefaultValue: 2
			text: qsTr("Sum of squares")
			model: ListModel
			{    
				ListElement { title: "Type \u2160"; value: "type1"}
				ListElement { title: "Type \u2161"; value: "type2"}
				ListElement { title: "Type \u2162"; value: "type3"}
			}
		}
		
	}
	
	ExpanderButton
	{
		title: qsTr("Assumption Checks")
		
		CheckBox { name: "homogeneityTests";		text: qsTr("Homogeneity tests")										}
		CheckBox { name: "homogeneityCorrections";	text: qsTr("Homogeneity corrections"); id: homogeneityCorrections	}
		RowLayout
		{
			Layout.leftMargin: Theme.indentationLength
			enabled: homogeneityCorrections.checked
			CheckBox { name: "homogeneityNone";		text: qsTr("None")           ; checked: true }
			CheckBox { name: "homogeneityBrown";	text: qsTr("Brown-Forsythe") ; checked: true }
			CheckBox { name: "homogeneityWelch";	text: qsTr("Welch")          ; checked: true }
		}
		CheckBox { name: "qqPlot"; text: qsTr("Q-Q plot of residuals") }
	}
	
	ExpanderButton
	{
		title: qsTr("Contrasts")
		
		ContrastsList { syncModels: ["fixedFactors", "randomFactors"] }
		
		CheckBox { name: "contrastAssumeEqualVariance"; text: qsTr("Assume equal variances"); checked: true }
		RowLayout
		{
			CheckBox {		name: "confidenceIntervalsContrast"; text: qsTr("Confidence intervals"); id: confidenceIntervalsContrast }
			PercentField {	name: "confidenceIntervalIntervalContrast"; defaultValue: 95; enabled: confidenceIntervalsContrast.checked }
		}
	}
	
	ExpanderButton
	{
		title: qsTr("Post Hoc Tests")
		
		VariablesForm
		{
			height: 200
			availableVariablesList { name: "postHocTestsAvailable"; syncModels: "fixedFactors" }
			AssignedVariablesList {  name: "postHocTestsVariables" }
		}
		
		GridLayout
		{
			CheckBox {			name: "postHocTestEffectSize"; text: qsTr("Effect Size") }
			
			RowLayout
			{
				CheckBox {		name: "confidenceIntervalsPostHoc"; text: qsTr("Confidence intervals"); id: confidenceIntervalsPostHoc }
				PercentField {	name: "confidenceIntervalIntervalPostHoc"; defaultValue: 95; enabled: confidenceIntervalsPostHoc.checked }
			}
			
			GroupBox
			{
				title: qsTr("Correction")
				CheckBox { name: "postHocTestsTukey";		text: qsTr("Tukey"); checked: true	}
				CheckBox { name: "postHocTestsScheffe";		text: qsTr("Scheffe")				}
				CheckBox { name: "postHocTestsBonferroni";	text: qsTr("Bonferroni")			}
				CheckBox { name: "postHocTestsHolm";		text: qsTr("Holm")					}
			}
			
			GroupBox
			{
				title: qsTr("Type")
				CheckBox { name: "postHocTestsTypeStandard";	text: qsTr("Standard"); checked: true	}
				CheckBox { name: "postHocTestsTypeGames";		text: qsTr("Games-Howell")				}
				CheckBox { name: "postHocTestsTypeDunnett";		text: qsTr("Dunnett")					}
				CheckBox { name: "postHocTestsTypeDunn";		text: qsTr("Dunn")						}
			}
		}
	}
	
	ExpanderButton
	{
		title: qsTr("Descriptives Plots")
		
		VariablesForm
		{
			height: 170
			availableVariablesList { name: "descriptivePlotsVariables"; title: qsTr("Factors"); syncModels: ["fixedFactors", "randomFactors"] }
			AssignedVariablesList { name: "plotHorizontalAxis";			title: qsTr("Horizontal axis"); singleItem: true }
			AssignedVariablesList { name: "plotSeparateLines";			title: qsTr("Separate lines");	singleItem: true }
			AssignedVariablesList { name: "plotSeparatePlots";			title: qsTr("Separate plots");	singleItem: true }
		}
		
		GroupBox
		{
			title: qsTr("Display")
			CheckBox { name: "plotErrorBars"; text: qsTr("Display error bars") }
			
			RadioButtonGroup
			{
				name: "errorBarType"
				RadioButton { value: "confidenceInterval";			text: qsTr("Confidence Interval"); checked: true; id: confidenceInterval }
				PercentField { name: "confidenceIntervalInterval";	text: qsTr("Interval"); defaultValue: 95; indent: true; enabled: confidenceInterval.checked}
				RadioButton { value: "standardError";				text: qsTr("Standard error") }
			}
		}
	}
	
	ExpanderButton
	{
		title: qsTr("Additional Options")
		
		Label { text: qsTr("Marginal means") }
		VariablesForm
		{
			height: 200
			availableVariablesList { name: "marginalMeansTermsAvailable" ; syncModels: "modelTerms"; showVariableTypeIcon: false }
			AssignedVariablesList {  name: "marginalMeansTerms"; showVariableTypeIcon: false }
		}
		
		CheckBox { name: "marginalMeansCompareMainEffects"; text: qsTr("Compare marginal means to 0"); id: marginalMeansCompareMainEffects }
		DropDown
		{
			name: "marginalMeansCIAdjustment"
			indent: true
			text: qsTr("Confidence interval adjustment")
			model: ListModel
			{
				ListElement { title: "None";		value: "none"		}
				ListElement { title: "Bonferro";	value: "bonferroni"	}
				ListElement { title: "Sidak";		value: "sidak"		}
			}
			enabled: marginalMeansCompareMainEffects.checked
		}
		
		GroupBox
		{
			title: qsTr("Display")
			CheckBox { name: "descriptives";		text: qsTr("Descriptive statistics")							}
			CheckBox { name: "effectSizeEstimates";	text: qsTr("Estimates of effect size"); id: effectSizeEstimates	}
			Row
			{
				Layout.leftMargin: Theme.indentationLength
				enabled: effectSizeEstimates.checked
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
			height: 170
			availableVariablesList { name: "effectsVariables";	title: qsTr("Factors")				; syncModels:  ["fixedFactors", "randomFactors"] }
			AssignedVariablesList {	name: "simpleFactor";		title: qsTr("Simple effect factor") ; singleItem: true }
			AssignedVariablesList { name: "moderatorFactorOne";	title: qsTr("Moderator factor 1")	; singleItem: true }
			AssignedVariablesList { name: "moderatorFactorTwo";	title: qsTr("Moderator factor 2")	; singleItem: true }
		}
	}
	
	ExpanderButton
	{
		title: qsTr("Nonparametrics")
		
		VariablesForm
		{
			height: 200
			availableVariablesList { name: "kruskalVariablesAvailable"; title: qsTr("Kruskal-Wallis test") ; syncModels:  ["fixedFactors", "randomFactors"] }
			AssignedVariablesList {  name: "kruskalVariablesAssigned" }
		}
	}	
}
