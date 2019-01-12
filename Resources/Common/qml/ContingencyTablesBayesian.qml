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
	id: form
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
		AssignedVariablesList {  name: "layers";	title: qsTr("Layers");	allowedColumns: ["ordinal", "nominal"]; listViewType: "Layers"; height: 120 }
	}
	
	ExpanderButton
	{
		text: qsTr("Statistics")
		
		GridLayout
		{
			RadioButtonGroup
			{
				title: qsTr("Sample")
				name: "samplingModel"
				RadioButton { text: qsTr("Poisson")							; name: "poisson" }
				RadioButton { text: qsTr("Joint multinomial")				; name: "jointMultinomial" }
				RadioButton { text: qsTr("Indep. multinomial, rows fixed")	; name: "independentMultinomialRowsFixed" ; checked: true }
				RadioButton { text: qsTr("Indep. multinomial, columns fixed") ; name: "independentMultinomialColumnsFixed" }
				RadioButton { text: qsTr("Hypergeometrics (2x2 only)")		; name: "hypergeometric"; id: hypergeometric }
			}
			
			GroupBox
			{
				title: qsTr("Additional Statistics")
				CheckBox { text: qsTr("Log odds ratio (2x2 only)") ; name: "oddsRatio" ; id: oddsRatio }
				PercentField { text: qsTr("Credible interval") ; name: "oddsRatioCredibleIntervalInterval"; defaultValue: 95; enabled: oddsRatio.checked; indent: true }
				CheckBox { text: qsTr("Cramer's V") ; name: "effectSize" ; id: effectSize ; debug: true }
				PercentField { text: qsTr("Credible interval") ; name: "effectSizeCredibleIntervalInterval"; defaultValue: 95; enabled: effectSize.checked; indent: true ; debug: true }
			}
			
			RadioButtonGroup
			{
				title: qsTr("Hypothesis")
				name: "hypothesis"
				enabled: !hypergeometric.checked
				RadioButton { text: qsTr("Group one ≠ Group two"); name: "groupsNotEqual" ; checked: true }
				RadioButton { text: qsTr("Group one > Group two"); name: "groupOneGreater" ; }
				RadioButton { text: qsTr("Group one < Group two"); name: "groupTwoGreater" ; }
			}
			
			GroupBox
			{
				title: qsTr("Plots")
				CheckBox { text: qsTr("Log odds ratio (2x2 only)")  ; name: "plotPosteriorOddsRatio"; id: plotPosteriorOddsRatio }
				CheckBox { text: qsTr("Additional info")            ; name: "plotPosteriorOddsRatioAdditionalInfo" ; checked: true ; enabled: plotPosteriorOddsRatio.checked }
				CheckBox { text: qsTr("Cramer's V") ; name: "plotPosteriorEffectSize" ; debug: true }
			}
			
			BayesFactorType {}
			
			GroupBox
			{
				title: qsTr("Prior")
				DoubleField { text: qsTr("Prior concentration") ; name: "priorConcentration" ; defaultValue: 1 ; doubleValidator { bottom: 1; decimals: 1 } }
			}
		}
	}
	
	ExpanderButton
	{
		text: qsTr("Options")
		
		GridLayout
		{
			RadioButtonGroup
			{
				title: qsTr("Row Order")
				name: "rowOrder"
				RadioButton { text: qsTr("Ascending"); name: "ascending"; checked: true }
				RadioButton { text: qsTr("Descending"); name: "descending" }
			}
			RadioButtonGroup
			{
				title: qsTr("Column Order")
				name: "columnOrder"
				RadioButton { text: qsTr("Ascending"); name: "ascending"; checked: true }
				RadioButton { text: qsTr("Descending"); name: "descending" }
			}
		}
	}
}
