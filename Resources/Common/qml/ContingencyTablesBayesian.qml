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

Form
{
	usesJaspResults: false
	plotWidth: 320
	plotHeight: 240
	
	CheckBox { visible: false ; name: "bfIndependence"; checked: false }
	CheckBox { visible: false ; name: "countsExpected"; checked: false }
	CheckBox { visible: false ; name: "countsObserved"; checked: false }
	CheckBox { visible: false ; name: "hideSmallCounts"; checked: false }
	IntegerField { visible: false ; name: "hideSmallCountsLessThan" ; defaultValue: 5 }
	CheckBox { visible: false ; name: "percentagesColumn"; checked: false }
	CheckBox { visible: false ; name: "percentagesRow"; checked: false }
	CheckBox { visible: false ; name: "percentagesTotal"; checked: false }
	CheckBox { visible: false ; name: "residualsAdjustedStandardized"; checked: false }
	CheckBox { visible: false ; name: "residualsStandardized"; checked: false }
	CheckBox { visible: false ; name: "residualsUnstandardized"; checked: false }
	CheckBox { visible: false ; name: "zTestAdjustPValues"; checked: false }
	CheckBox { visible: false ; name: "zTestCompareColumns"; checked: false }
	
	VariablesForm
	{
		AssignedVariablesList { name: "rows";		title: qsTr("Rows");	allowedColumns: ["ordinal", "nominal"] }
		AssignedVariablesList { name: "columns";	title: qsTr("Columns");	allowedColumns: ["ordinal", "nominal"] }
		AssignedVariablesList { name: "counts";		title: qsTr("Counts");	allowedColumns: ["scale"]; singleItem: true }
		AssignedVariablesList { name: "layers";		title: qsTr("Layers");	allowedColumns: ["ordinal", "nominal"]; listViewType: "Layers"; height: 120 }
	}
	
	ExpanderButton
	{
		title: qsTr("Statistics")
		
		GridLayout
		{
			RadioButtonGroup
			{
				name: "samplingModel"
				title: qsTr("Sample")
				RadioButton { value: "poisson";							text: qsTr("Poisson")										}
				RadioButton { value: "jointMultinomial";				text: qsTr("Joint multinomial")								}
				RadioButton { value: "independentMultinomialRowsFixed";	text: qsTr("Indep. multinomial, rows fixed"); checked: true	}
				RadioButton { value: "independentMultinomialColumnsFixed"; text: qsTr("Indep. multinomial, columns fixed")			}
				RadioButton { value: "hypergeometric";					text: qsTr("Hypergeometrics (2x2 only)"); id: hypergeometric }
			}
			
			GroupBox
			{
				title: qsTr("Additional Statistics")
				CheckBox { name: "oddsRatio";	text: qsTr("Log odds ratio (2x2 only)"); id: oddsRatio }
				PercentField { name: "oddsRatioCredibleIntervalInterval"; text: qsTr("Credible interval"); defaultValue: 95; enabled: oddsRatio.checked; indent: true }
				CheckBox { name: "effectSize"; text: qsTr("Cramer's V"); id: effectSize; debug: true }
				PercentField { name: "effectSizeCredibleIntervalInterval"; text: qsTr("Credible interval");  defaultValue: 95; enabled: effectSize.checked; indent: true ; debug: true }
			}
			
			RadioButtonGroup
			{
				title: qsTr("Hypothesis")
				name: "hypothesis"
				enabled: !hypergeometric.checked
				RadioButton { value: "groupsNotEqual";	text: qsTr("Group one ≠ Group two"); checked: true	}
				RadioButton { value: "groupOneGreater";	text: qsTr("Group one > Group two")					}
				RadioButton { value: "groupTwoGreater";	text: qsTr("Group one < Group two")					}
			}
			
			GroupBox
			{
				title: qsTr("Plots")
				CheckBox { name: "plotPosteriorOddsRatio";				text: qsTr("Log odds ratio (2x2 only)"); id: plotPosteriorOddsRatio }
				CheckBox { name: "plotPosteriorOddsRatioAdditionalInfo"; text: qsTr("Additional info"); checked: true; enabled: plotPosteriorOddsRatio.checked }
				CheckBox { name: "plotPosteriorEffectSize";				text: qsTr("Cramer's V"); debug: true }
			}
			
			BayesFactorType {}
			
			GroupBox
			{
				title: qsTr("Prior")
				DoubleField { name: "priorConcentration"; text: qsTr("Prior concentration"); defaultValue: 1; doubleValidator { bottom: 1; decimals: 1 } }
			}
		}
	}
	
	ExpanderButton
	{
		title: qsTr("Options")
		
		GridLayout
		{
			RadioButtonGroup
			{
				name: "rowOrder"
				title: qsTr("Row Order")
				RadioButton { value: "ascending";	text: qsTr("Ascending"); checked: true	}
				RadioButton { value: "descending";	text: qsTr("Descending")				}
			}
			RadioButtonGroup
			{
				name: "columnOrder"
				title: qsTr("Column Order")
				RadioButton { value: "ascending";	text: qsTr("Ascending"); checked: true	}
				RadioButton { value: "descending";	text: qsTr("Descending")				}
			}
		}
	}
}
