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

Form
{
	VariablesForm
	{
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList { name: "variablesList" }
		AssignedVariablesList {
			name: "variables"
			title: qsTr("Variables")
			allowedColumns: ["scale"]
		}
	}

	ColumnLayout
	{
		DoubleField
		{
			text: qsTr("Test value")
			defaultValue: 0
			name: "testValue"
			negativeValues: true
			decimals: 2
		}

		RadioButtonGroup
		{
			title: qsTr("Hypothesis Test")
			name: "hypothesis"

			RadioButton { text: qsTr("Equal vs. not equal")                     ; name: "equalNotEqual" ; checked: true}
			RadioButton { text: qsTr("Equal vs. bigger")                        ; name: "equalBigger" }
			RadioButton { text: qsTr("Equal vs. smaller")                       ; name: "equalSmaller" }
			RadioButton { text: qsTr("Bigger vs. smaller")                      ; name: "biggerSmaller" }
			RadioButton { text: qsTr("Equal vs. bigger vs. smaller")            ; name: "equalBiggerSmaller" }
		}

		RadioButtonGroup
		{
			title: qsTr("Bayes Factor")
			name: "bayesFactorType"

			RadioButton { text: qsTr("BF\u2080\u2081: Equal vs. other")         ; name: "BF01" ; checked: true}
			RadioButton { text: qsTr("BF\u2081\u2080: Other vs. equal")         ; name: "BF10" }
		}

		Group
		{
			title: qsTr("Additional Options")

			DoubleField  { 
				name: "seed"
				text: qsTr("Seed")
				defaultValue: 100
				min: -999999
				max: 999999
				fieldWidth: 60 
			}
		}
	}

	ColumnLayout
	{
		Group
		{
			title: qsTr("Tables")

			CheckBox { name: "descriptives"; text: qsTr("Descriptives") 
				CIField { name: "credibleInterval"; text: qsTr("Credible interval")}
			}
		}

		Group
		{
			title: qsTr("Plots")

			CheckBox { name: "bayesFactorPlot"; text: qsTr("Posterior probabilities") }
			CheckBox { name: "descriptivesPlots"; text: qsTr("Descriptives plots") }
		}
	}
}
