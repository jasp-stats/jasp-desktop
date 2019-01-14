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
	
	VariablesForm
	{
		AssignedVariablesList { name: "counts";	 title: qsTr("Counts (optional)");	singleItem: true	}
		AssignedVariablesList { name: "factors"; title: qsTr("Factors"); itemType: "fixedFactors"; allowedColumns: ["ordinal", "nominal"] }
	}
	
	GridLayout
	{
		columns: 3
		BayesFactorType {}
		
		GroupBox
		{
			title: qsTr("Prior")
			DoubleField { name: "priorShape"; text: qsTr("Shape"); defaultValue: -1 ; doubleValidator.bottom: -1 }
			DoubleField { name: "priorScale"; text: qsTr("Scale"); defaultValue: 0 }
		}
		
		GroupBox
		{
			title: qsTr("Model cut-offs")
			IntegerField { name: "maxModels";					text: qsTr("Display best") ;  defaultValue: 2; afterLabel.text: qsTr("models"); intValidator.bottom: 2 }
			DoubleField { name: "posteriorProbabilityCutOff";	text: qsTr("Posterior prob."); defaultValue: 0.1 ; doubleValidator.top: 0.5 }
		}
	}
	
	ExpanderButton
	{
		title: qsTr("Model")
		
		VariablesForm
		{
			height: 200
			listWidth: parent.width * 5 / 9
			
			availableVariablesList { name: "availableTerms"; title: qsTr("Components"); width: parent.width / 4; syncModels: ['factors'] }
			AssignedVariablesList {  name: "modelTerms";	 title: qsTr("Model terms"); listViewType: "Interaction" }
		}
	}
	
	ExpanderButton
	{
		title: qsTr("Statistics")
		
		GridLayout
		{
			GroupBox
			{
				title: qsTr("Regression Coefficients")
				CheckBox { name: "regressionCoefficientsEstimates";			text: qsTr("Estimates")							}
				CheckBox { name: "regressionCoefficientsCredibleIntervals";	text: qsTr("Credible intervals"); id: interval	}
				PercentField
				{
					name: "regressionCoefficientsCredibleIntervalsInterval"
					text: qsTr("Interval");
					defaultValue: 95
					validator: IntValidator {bottom: 50}
					enabled: interval.checked
					indent: true
				}
			}
			
			GroupBox
			{
				Row
				{
					CheckBox { name: "regressionCoefficientsSubmodel"; text: qsTr("Submodel") ; id: regressionCoefficientsSubmodel }
					IntegerField { name: "regressionCoefficientsSubmodelNo"; defaultValue: 1 ; intValidator.bottom: 1; enabled: regressionCoefficientsSubmodel.checked }
				}
				
				GroupBox
				{
					indent: true;
					enabled: regressionCoefficientsSubmodel.checked
					CheckBox { name: "regressionCoefficientsSubmodelEstimates"; text: qsTr("Estimates") }
					CheckBox { name: "regressionCoefficientsSubmodelCredibleIntervals"; text: qsTr("Credible intervals"); id: submodelCredibleIntervals }
					PercentField { name: "regressionCoefficientsSubmodelCredibleIntervalsInterval"; text: qsTr("Interval"); defaultValue: 95 ; enabled: submodelCredibleIntervals.checked ; indent: true }
				}
			}
		}
	}
	
	ExpanderButton
	{
		title: qsTr("Advanced")
		
		RadioButtonGroup
		{
			title: qsTr("Samples")
			name: "sampleMode"
			RadioButton { value: "auto";	text: qsTr("Auto");  checked: true	}
			RadioButton { value: "manual";	text: qsTr("Manual"); id: manual	}
			IntegerField { name: "fixedSamplesNumber"; text: qsTr("No. samples"); defaultValue: 10000; fieldWidth: 60; enabled: manual.checked; indent: true }
		}
	}
}
