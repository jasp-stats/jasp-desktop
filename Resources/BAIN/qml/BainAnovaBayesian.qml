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
		AvailableVariablesList { name: "variablesList"}
		AssignedVariablesList {
			name: "dependent"
			title: qsTr("Dependent Variable")
			singleVariable: true
			suggestedColumns: ["scale"]
		}

		AssignedVariablesList {
			name: "fixedFactors"
			title: qsTr("Fixed Factors")
			singleVariable: true
			suggestedColumns: ["ordinal", "nominal"]
		}

	}

	Group
	{
		title: qsTr("Tables")

		CheckBox { name: "bayesFactorMatrix"; text: qsTr("Bayes factor matrix") }
		CheckBox
		{
			name: "descriptives"
			text: qsTr("Descriptives")
			CIField { name: "CredibleInterval"; text: qsTr("Credible interval") }
		}
	}

	Group
	{
		title: qsTr("Plots")
		CheckBox { name: "bayesFactorPlot";		text: qsTr("Bayes factor comparison")	}
		CheckBox { name: "descriptivesPlot";	text: qsTr("Descriptives plot")			}
	}

	Section
	{
		text: qsTr("Model Constraints")
		columns: 1

		Text { text: "Place each hypothesis on a new line. For example:\n\nfactor.low = factor.med = factor.high\nfactor.low < factor.med < factor.high\n\nwhere factor is the factor name and low/med/high are the factor level names.\nRead the help file for further instructions." }
		TextArea
		{
				name: "model"
				implicitHeight: 200
				infoText: Qt.platform.os == "osx" ? "\u2318 + Enter to apply" : "Ctrl + Enter to apply"
				text: ""
				textType: "model"
				trim: true
		}
	}
}
