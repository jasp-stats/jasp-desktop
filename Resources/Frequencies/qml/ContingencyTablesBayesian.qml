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
		AvailableVariablesList { name: "allVariablesList" }		
		AssignedVariablesList { name: "rows";		title: qsTr("Rows");	suggestedColumns: ["ordinal", "nominal"] }
		AssignedVariablesList { name: "columns";	title: qsTr("Columns");	suggestedColumns: ["ordinal", "nominal"] }
        AssignedVariablesList { name: "counts";		title: qsTr("Counts");	suggestedColumns: ["scale", "ordinal"]; singleVariable: true }
		AssignedVariablesList { name: "layers";		title: qsTr("Layers");	suggestedColumns: ["ordinal", "nominal"]; listViewType: "Layers"; height: 120 }
	}
	
	Section
	{
		title: qsTr("Statistics")
		
		RadioButtonGroup
		{
			name: "samplingModel"
			title: qsTr("Sample")
			RadioButton { value: "poisson";							label: qsTr("Poisson")											}
			RadioButton { value: "jointMultinomial";				label: qsTr("Joint multinomial")								}
			RadioButton { value: "independentMultinomialRowsFixed";	label: qsTr("Indep. multinomial, rows fixed"); checked: true	}
			RadioButton { value: "independentMultinomialColumnsFixed"; label: qsTr("Indep. multinomial, columns fixed")				}
			RadioButton { value: "hypergeometric";					label: qsTr("Hypergeometrics (2x2 only)"); id: hypergeometric	}
		}

		Group
		{
			title: qsTr("Additional Statistics")
			CheckBox
			{
				name: "oddsRatio";	label: qsTr("Log odds ratio (2x2 only)")
				CIField { name: "oddsRatioCredibleIntervalInterval"; label: qsTr("Credible interval") }
			}
			CheckBox
			{
				name: "effectSize"; label: qsTr("Cramer's V"); debug: true
				CIField { name: "effectSizeCredibleIntervalInterval"; label: qsTr("Credible interval"); debug: true }
			}
		}

		RadioButtonGroup
		{
			title: qsTr("Alt. Hypothesis")
			name: "hypothesis"
			enabled: !hypergeometric.checked
			RadioButton { value: "groupsNotEqual";	label: qsTr("Group one ≠ Group two"); checked: true	}
			RadioButton { value: "groupOneGreater";	label: qsTr("Group one > Group two")					}
			RadioButton { value: "groupTwoGreater";	label: qsTr("Group one < Group two")					}
		}

		Group
		{
			title: qsTr("Plots")
			CheckBox
			{
				name: "plotPosteriorOddsRatio";			label: qsTr("Log odds ratio (2x2 only)")
				CheckBox { name: "plotPosteriorOddsRatioAdditionalInfo"; label: qsTr("Additional info"); checked: true }
			}
			CheckBox { name: "plotPosteriorEffectSize";	label: qsTr("Cramer's V"); debug: true }
		}

		BayesFactorType {}

		Group
		{
			title: qsTr("Prior")
			DoubleField { name: "priorConcentration"; label: qsTr("Prior concentration"); defaultValue: 1; min: 1; decimals: 1 }
		}
	}
	
	Section
	{
		title: qsTr("Options")
		
		RadioButtonGroup
		{
			name: "rowOrder"
			title: qsTr("Row Order")
			RadioButton { value: "ascending";	label: qsTr("Ascending"); checked: true	}
			RadioButton { value: "descending";	label: qsTr("Descending")				}
		}
		RadioButtonGroup
		{
			name: "columnOrder"
			title: qsTr("Column Order")
			RadioButton { value: "ascending";	label: qsTr("Ascending"); checked: true	}
			RadioButton { value: "descending";	label: qsTr("Descending")				}
		}
	}
}
