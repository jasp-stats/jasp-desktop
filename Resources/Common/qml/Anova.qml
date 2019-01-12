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
	id: form
	
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
		text: qsTr("Model")
		
		VariablesForm
		{
			height: 200
			availableVariablesList { name: "components"; title: qsTr("Components"); syncModels: ["fixedFactors", "randomFactors"] }
			AssignedVariablesList {  name: "modelTerms"; title: qsTr("Model terms"); listViewType: "Interaction" }
		}
		
		ComboBox
		{
			name: "sumOfSquares"
			currentIndex: 2
			text: qsTr("Sum of squares")
			model: ListModel
			{    
				ListElement { key: "Type \u2160"; value: "type1"}
				ListElement { key: "Type \u2161"; value: "type2"}
				ListElement { key: "Type \u2162"; value: "type3"}
			}
		}
		
	}
	
	ExpanderButton
	{
		text: qsTr("Assumption Checks")
		
		CheckBox { text: qsTr("Homogeneity tests")          ; name: "homogeneityTests" }
		CheckBox { text: qsTr("Homogeneity corrections")    ; name: "homogeneityCorrections" ; id: homogeneityCorrections }
		RowLayout
		{
			Layout.leftMargin: Theme.indentationLength
			enabled: homogeneityCorrections.checked
			CheckBox { text: qsTr("None")           ; name: "homogeneityNone" ; checked: true }
			CheckBox { text: qsTr("Brown-Forsythe") ; name: "homogeneityBrown" ; checked: true }
			CheckBox { text: qsTr("Welch")          ; name: "homogeneityWelch" ; checked: true }
		}
		CheckBox { text: qsTr("Q-Q plot of residuals") ; name: "qqPlot" }
	}
	
	ExpanderButton
	{
		text: qsTr("Contrasts")
		
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
		text: qsTr("Post Hoc Tests")
		
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
				CheckBox { text: qsTr("Tukey")          ; name: "postHocTestsTukey"     ; checked: true }
				CheckBox { text: qsTr("Scheffe")        ; name: "postHocTestsScheffe"                   }
				CheckBox { text: qsTr("Bonferroni")     ; name: "postHocTestsBonferroni"                }
				CheckBox { text: qsTr("Holm")           ; name: "postHocTestsHolm"                      }
			}
			
			GroupBox
			{
				title: qsTr("Type")
				CheckBox { text: qsTr("Standard")       ; name: "postHocTestsTypeStandard" ; checked: true }
				CheckBox { text: qsTr("Games-Howell")   ; name: "postHocTestsTypeGames"                 }
				CheckBox { text: qsTr("Dunnett")        ; name: "postHocTestsTypeDunnett"               }
				CheckBox { text: qsTr("Dunn")           ; name: "postHocTestsTypeDunn"                  }
			}
		}
	}
	
	ExpanderButton
	{
		text: qsTr("Descriptives Plots")
		
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
			CheckBox { text: qsTr("Display error bars"); name: "plotErrorBars" }
			
			RadioButtonGroup
			{
				name: "errorBarType"
				RadioButton { text: qsTr("Confidence Interval"); name: "confidenceInterval"; checked: true; id: confidenceInterval }
				PercentField { indent: true; text: qsTr("Interval"); name: "confidenceIntervalInterval"; defaultValue: 95; enabled: confidenceInterval.checked}
				RadioButton { text: qsTr("Standard error"); name: "standardError" }
			}
		}
	}
	
	ExpanderButton
	{
		text: qsTr("Additional Options")
		Label { text: qsTr("Marginal means") }
		
		VariablesForm
		{
			height: 200
			availableVariablesList { name: "marginalMeansTermsAvailable" ; syncModels: "modelTerms"; showVariableTypeIcon: false }
			AssignedVariablesList {  name: "marginalMeansTerms"; showVariableTypeIcon: false }
		}
		
		CheckBox { text: qsTr("Compare marginal means to 0"); name: "marginalMeansCompareMainEffects"; id: marginalMeansCompareMainEffects }
		ComboBox
		{
			name: "marginalMeansCIAdjustment"
			indent: true
			text: qsTr("Confidence interval adjustment")
			model: ListModel
			{
				ListElement {key: "None"; value: "none"}
				ListElement {key: "Bonferro"; value: "bonferroni"}
				ListElement {key: "Sidak"; value: "sidak"}
			}
			enabled: marginalMeansCompareMainEffects.checked
		}
		
		GroupBox
		{
			title: qsTr("Display")
			CheckBox { text: qsTr("Descriptive statistics")     ; name: "descriptives" }
			CheckBox { text: qsTr("Estimates of effect size")   ; name: "effectSizeEstimates"   ; id: effectSizeEstimates }
			Row
			{
				Layout.leftMargin: Theme.indentationLength
				enabled: effectSizeEstimates.checked
				CheckBox { text: qsTr("η²")         ; name: "effectSizeEtaSquared"; checked: true }
				CheckBox { text: qsTr("partial η²") ; name: "effectSizePartialEtaSquared" }
				CheckBox { text: qsTr("ω²")         ; name: "effectSizeOmegaSquared" }
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
			availableVariablesList { name: "effectsVariables";	title: qsTr("Factors")				; syncModels:  ["fixedFactors", "randomFactors"] }
			AssignedVariablesList {	name: "simpleFactor";		title: qsTr("Simple effect factor") ; singleItem: true }
			AssignedVariablesList { name: "moderatorFactorOne";	title: qsTr("Moderator factor 1")	; singleItem: true }
			AssignedVariablesList { name: "moderatorFactorTwo";	title: qsTr("Moderator factor 2")	; singleItem: true }
		}
	}
	
	ExpanderButton
	{
		text: qsTr("Nonparametrics")
		
		VariablesForm
		{
			height: 200
			availableVariablesList { name: "kruskalVariablesAvailable"; title: qsTr("Kruskal-Wallis test") ; syncModels:  ["fixedFactors", "randomFactors"] }
			AssignedVariablesList {  name: "kruskalVariablesAssigned" }
		}
	}	
}
