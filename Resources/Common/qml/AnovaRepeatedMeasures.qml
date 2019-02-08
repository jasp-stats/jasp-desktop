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
	
	IntegerField { visible: false; name: "plotHeightDescriptivesPlotLegend"     ; defaultValue: 300 }
	IntegerField { visible: false; name: "plotHeightDescriptivesPlotNoLegend"   ; defaultValue: 300 }
	IntegerField { visible: false; name: "plotWidthDescriptivesPlotLegend"      ; defaultValue: 450 }
	IntegerField { visible: false; name: "plotWidthDescriptivesPlotNoLegend"    ; defaultValue: 350 }
	
	
	VariablesForm
	{
		height: 520
		RepeatedMeasuresFactorsList { name: "repeatedMeasuresFactors"; title: qsTr("Repeated Measures Factors"); height: 180 }
		AssignedVariablesList
		{
			name: "repeatedMeasuresCells"
			title: qsTr("Repeated Measures Cells")
			allowedColumns: ["scale"]
			listViewType: "MeasuresCells"
			source: "repeatedMeasuresFactors"
			height: 140
		}
		AssignedVariablesList
		{
			name: "betweenSubjectFactors"
			title: qsTr("Between Subject Factors")
			allowedColumns: ["ordinal", "nominal"]
			itemType: "fixedFactors"
		}
		AssignedVariablesList
		{
			name: "covariates"
			title: qsTr("Covariates")
			allowedColumns: ["scale"]
		}
	}
	
	
	ExpanderButton
	{
		title: qsTr("Model")
		
		VariablesForm
		{
			height: 150
			availableVariablesList { name: "withinComponents"; title: qsTr("Repeated Measures Components"); source: ["repeatedMeasuresFactors"] }
			AssignedVariablesList {  name: "withinModelTerms"; title: qsTr("Model terms"); listViewType: "Interaction" }
		}
		
		VariablesForm
		{
			height: 150
			availableVariablesList { name: "betweenComponents"; title: qsTr("Between Subjects Components"); source: ["betweenSubjectFactors", "covariates"] }
			AssignedVariablesList {  name: "betweenModelTerms"; title: qsTr("Model terms"); listViewType: "Interaction" }
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
			CheckBox { name: "sphericityTests";	text: qsTr("Sphericity tests") }
			CheckBox
			{
				name: "sphericityCorrections";	text: qsTr("Sphericity corrections")
				columns: 3
				CheckBox { name: "sphericityNone";				text: qsTr("None");					checked: true }
				CheckBox { name: "sphericityGreenhouseGeisser";	text: qsTr("Greenhouse-Geisser");	checked: true }
				CheckBox { name: "sphericityHuynhFeldt";		text: qsTr("Huynth-Feidt");			checked: true }
			}
			CheckBox { name: "homogeneityTests"; text: qsTr("Homogeneity Tests") }
		}
	}
	
	ExpanderButton
	{
		title: qsTr("Contrasts")
		ContrastsList { source: ["repeatedMeasuresFactors", "betweenSubjectFactors"] }
	}
	
	ExpanderButton
	{
		title: qsTr("Post Hoc Tests")
		columns: 1

		VariablesForm
		{
			height: 150
			availableVariablesList { name: "postHocTestsAvailable"; source: ["repeatedMeasuresFactors", "betweenSubjectFactors"] }
			AssignedVariablesList {  name: "postHocTestsVariables" }
		}
		
		Group
		{
			columns: 2
			CheckBox { name: "postHocTestEffectSize";	text: qsTr("Effect Size")						}
			CheckBox { name: "postHocTestPooledError";	text: qsTr("Pool error term for RM factors")	}
		}
		
		Group
		{
			title: qsTr("Correction")
			CheckBox { name: "postHocTestsTukey";		text: qsTr("Tukey"); checked: true	}
			CheckBox { name: "postHocTestsScheffe";		text: qsTr("Scheffe")				}
			CheckBox { name: "postHocTestsBonferroni";	text: qsTr("Bonferroni")			}
			CheckBox { name: "postHocTestsHolm";		text: qsTr("Holm")					}
		}
	}
	
	ExpanderButton
	{
		title: qsTr("Descriptives Plots")
		columns: 1
		
		VariablesForm
		{
			height: 150
			availableVariablesList { name: "descriptivePlotsVariables"; title: qsTr("Factors");			source: ["repeatedMeasuresFactors", "betweenSubjectFactors"] }
			AssignedVariablesList {  name: "plotHorizontalAxis";		title: qsTr("Horizontal axis"); singleItem: true }
			AssignedVariablesList {  name: "plotSeparateLines";			title: qsTr("Separate lines");	singleItem: true }
			AssignedVariablesList {  name: "plotSeparatePlots";			title: qsTr("Separate plots");	singleItem: true }
		}
		
		TextField { name: "labelYAxis"; text: qsTr("Label y-axis"); fieldWidth: 200 }
		Group
		{
			title: qsTr("Display")
			columns: 2
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
						PercentField { name: "confidenceIntervalInterval"; text: qsTr("Interval"); defaultValue: 95 }
					}
					RadioButton { value: "standardError"; text: qsTr("Standard error") }
				}
			}
			CheckBox { name: "usePooledStandErrorCI";	text: qsTr("Pool SE across RM factors")	}
			
		}
	}
	
	ExpanderButton
	{
		title: qsTr("Additional Options")
		columns: 1

		Group
		{
			title: qsTr("Marginal means")
			debug: true
			
			VariablesForm
			{
				height: 150
				availableVariablesList { name: "marginalMeansTermsAvailable" ; source: "withinModelTerms" }
				AssignedVariablesList {  name: "marginalMeansTerms" }
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
		}
		
		Group
		{
			title: qsTr("Display")
			CheckBox { name: "descriptives";		text: qsTr("Descriptive statistics") }
			CheckBox
			{
				name: "effectSizeEstimates";	text: qsTr("Estimates of effect size")
				columns: 3
				CheckBox { name: "effectSizeEtaSquared";		text: qsTr("η²")         ; checked: true	}
				CheckBox { name: "effectSizePartialEtaSquared";	text: qsTr("partial η²")					}
				CheckBox { name: "effectSizeOmegaSquared";		text: qsTr("ω²")							}
			}
			CheckBox { name: "VovkSellkeMPR";					text: qsTr("Vovk-Sellke maximum p-ratio")	}
		}
	}
	
	ExpanderButton
	{
		title: qsTr("Simple Main Effects")
		
		VariablesForm
		{
			height: 150
			availableVariablesList { name: "effectsVariables";	title: qsTr("Factors"); source: ["repeatedMeasuresFactors", "betweenSubjectFactors"] }
			AssignedVariablesList {  name: "simpleFactor";		title: qsTr("Simple effect factor");	singleItem: true }
			AssignedVariablesList { name: "moderatorFactorOne";	title: qsTr("Moderator factor 1");		singleItem: true }
			AssignedVariablesList { name: "moderatorFactorTwo";	title: qsTr("Moderator factor 2");		singleItem: true }
		}
		
		CheckBox { name: "poolErrorTermSimpleEffects"; text: qsTr("Pool error terms") }
	}
	
	ExpanderButton
	{
		title: qsTr("Nonparametrics")
		
		VariablesForm
		{
			height: 150
			availableVariablesList { name: "kruskalVariablesAvailable"; title: qsTr("Factors"); source: ["repeatedMeasuresFactors", "betweenSubjectFactors"] }
			AssignedVariablesList {  name: "friedmanWithinFactor";		title: qsTr("RM Factor") }
			AssignedVariablesList {  name: "friedmanBetweenFactor";		title: qsTr("Optional grouping factor"); singleItem: true }
		}
		
		CheckBox { name: "connoverTest"; text: qsTr("Connover's post hoc tests") }
	}
}
