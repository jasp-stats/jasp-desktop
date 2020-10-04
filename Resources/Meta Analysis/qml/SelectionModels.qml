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

	RadioButtonGroup
	{
		Layout.columnSpan:		2
		name:					"measures"
		radioButtonsOnSameRow:	true
		columns:				2

		RadioButton
		{
			label: qsTr("Effect sizes & SE")
			value: "general"
			id: 	measures_general
			checked:true
		}

		RadioButton
		{
			label: qsTr("Correlations & N")
			value: "correlation"
			id: 	measures_correlation
		}
	}

	VariablesForm
	{
		preferredHeight: 200 * preferencesModel.uiScale

		AvailableVariablesList
		{
			name: "allVariables"
		}

		AssignedVariablesList
		{
			name:			"inputES"
			title:			if (measures_correlation.checked) {
				qsTr("Correlation")
			} else {
				qsTr("Effect Size")
			}
			singleVariable:	true
			allowedColumns:	["scale"]
		}

		AssignedVariablesList
		{
			name:			"inputSE"
			title:			qsTr("Effect Size Standard Error")
			singleVariable:	true
			allowedColumns:	["scale"]
			visible:		 measures_general.checked
			onVisibleChanged: if (!visible && count > 0) itemDoubleClicked(0);
		}

		AssignedVariablesList
		{
			name: 			"inputN"
			title: 			qsTr("N")
			singleVariable: true
			allowedColumns: ["scale", "ordinal"]
			visible:		 measures_correlation.checked
			onVisibleChanged: if (!visible && count > 0) itemDoubleClicked(0);
		}

		AssignedVariablesList
		{
			name:			"inputPVal"
			title:			qsTr("P-value (one-sided)")
			singleVariable:	true
			allowedColumns:	["scale"]
		}
	}

	Section
	{
		title: qsTr("Model")

		TextField
		{
			name:		"cutoffsPVal"
			text:		qsTr("P-value cutoffs")
			value:		"(.05, .10)"
			fieldWidth:	150
		}

		CheckBox
		{
			name:		"selectionTwosided"
			text:		qsTr("Two-sided selection")
			checked:	true
		}

		CheckBox
		{
			name:	"tablePVal"
			text:	qsTr("P-value frequency")
		}
		
		CheckBox
		{
			name:		"joinPVal"
			text:		qsTr("Automatically join p-value intervals")
			checked:	true
		}

		RadioButtonGroup
		{
			columns:	2
			name:		"effectDirection"
			title:		qsTr("Expected effect size direction")

			RadioButton
			{
				value:		"positive"
				label:		qsTr("Positive")
				checked: 	true
			}

			RadioButton
			{
				value:		"negative"
				label:		qsTr("Negative")
			}

		}

		DropDown
		{
			visible:	measures_correlation.checked
			label:		qsTr("Transform correlations")
			name:		"muTransform"
			values:
			[
				{ label: qsTr("Cohen's d"),		value: "cohensD"},
				{ label: qsTr("Fisher's z"),	value: "fishersZ"}
			]
		}
	}

	Section
	{
		title: qsTr("Inference")

		Group
		{
			title: qsTr("Fixed Effects")
			
			CheckBox
			{
				name:	"estimatesFE"
				text:	qsTr("Mean estimates")
				checked: true
			}

			CheckBox
			{
				name:	"weightsFE"
				text:	qsTr("Estimated weights")
			}
		
		}
		
		Group
		{
			title: qsTr("Random Effects")
		
			CheckBox
			{
				name:	"estimatesRE"
				text:	qsTr("Mean estimates")
				checked: true
			}

			CheckBox
			{
				name:	"heterogeneityRE"
				text:	qsTr("Estimated heterogeneity")
			}

			CheckBox
			{
				name:	"weightsRE"
				text:	qsTr("Estimated weights")
			}
	
		}

	}
	Section
	{
		title: qsTr("Plots")

		Group
		{
			title: qsTr("Weight Function")

			CheckBox
			{
				name:	"weightFunctionFE"
				text:	qsTr("Fixed effects")
			}

			CheckBox
			{
				name:	"weightFunctionRE"
				text:	qsTr("Random effects")
			}

			CheckBox
			{
				name:	"weightFunctionRescale"
				text:	qsTr("Rescale x-axis")
			}
		}

		CheckBox
		{
			name: "plotModels"
			text:	qsTr("Mean model estimates")
		}

	}
}
