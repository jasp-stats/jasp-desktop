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
import QtQuick			2.8
import QtQuick.Layouts	1.3
import JASP.Controls	1.0
import JASP.Widgets		1.0
import JASP				1.0

Form
{
	VariablesForm
	{
		AvailableVariablesList { name: "allVariablesList" }		
		AssignedVariablesList { name: "counts";	 title: qsTr("Counts (optional)");	singleVariable: true;	suggestedColumns:["scale", "ordinal"]}
		AssignedVariablesList { name: "factors"; title: qsTr("Factors"); itemType: "fixedFactors"; suggestedColumns: ["ordinal", "nominal"] }
	}
	
	columns: 3
	BayesFactorType { }

	Group
	{
		title: qsTr("Prior")
		DoubleField { name: "priorShape"; label: qsTr("Shape"); defaultValue: -1 ; min: -1 }
		DoubleField { name: "priorScale"; label: qsTr("Scale"); defaultValue: 0 }
	}

	Group
	{
		title: qsTr("Model Cut-offs")
		IntegerField { name: "maxModels";					label: qsTr("Display best") ;  defaultValue: 2; afterLabel: qsTr("models"); min: 2 }
		DoubleField { name: "posteriorProbabilityCutOff";	label: qsTr("Posterior prob."); defaultValue: 0.1 ; max: 0.5 }
	}
	
	Section
	{
		title: qsTr("Model")
		
		VariablesForm
		{
			preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
			
			AvailableVariablesList { name: "availableTerms"; title: qsTr("Components"); width: parent.width / 4; source: ['factors'] }
			AssignedVariablesList {  name: "modelTerms";	 title: qsTr("Model Terms"); width: parent.width * 5 / 9; listViewType: JASP.Interaction; addInteractionsByDefault: false }
		}
	}
	
	Section
	{
		title: qsTr("Statistics")
		
		Group
		{
			title: qsTr("Regression Coefficients")
			CheckBox { name: "regressionCoefficientsEstimates";	label: qsTr("Estimates") }
			CheckBox
			{
				name: "regressionCoefficientsCredibleIntervals"; label: qsTr("Credible intervals")
				CIField { name: "regressionCoefficientsCredibleIntervalsInterval"; label: qsTr("Interval") }
			}
		}

		Group
		{
			CheckBox
			{
				name: "regressionCoefficientsSubmodel"; label: qsTr("Submodel"); id: regressionCoefficientsSubmodel
				childrenOnSameRow: true
				IntegerField { name: "regressionCoefficientsSubmodelNo"; defaultValue: 1 ; min: 1 }
			}

			Group
			{
				indent: true;
				enabled: regressionCoefficientsSubmodel.checked
				CheckBox
				{
					name: "regressionCoefficientsSubmodelCredibleIntervals"; label: qsTr("Credible intervals")
					CIField { name: "regressionCoefficientsSubmodelCredibleIntervalsInterval"; label: qsTr("Interval") }
				}
			}
		}
	}
	
	Section
	{
		title: qsTr("Advanced")
		
		RadioButtonGroup
		{
			title: qsTr("Samples")
			name: "sampleMode"
			RadioButton { value: "auto";	label: qsTr("Auto");  checked: true	}
			RadioButton
			{
				value: "manual";			label: qsTr("Manual")
				IntegerField { name: "fixedSamplesNumber"; label: qsTr("No. samples"); defaultValue: 10000; fieldWidth: 60 }
			}
		}

		SetSeed{}

	}
}
