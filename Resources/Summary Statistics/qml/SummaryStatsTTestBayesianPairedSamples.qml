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

	GroupBox 
	{
		DoubleField  { text: qsTr("t");				name: "tStatistic" ; validation: false  }
		IntegerField { text: qsTr("Group size");	name: "n1Size" }
	}
	
    Divider { }

    GridLayout 
	{
		RadioButtonGroup 
		{
			title: qsTr("Hypothesis")
			name: "hypothesis"
			RadioButton { text: qsTr("Measure 1 \u2260 Measure 2"); name: "groupsNotEqual" ; checked: true }
			RadioButton { text: qsTr("Measure 1 > Measure 2")     ; name: "groupOneGreater"                }
			RadioButton { text: qsTr("Measure 1 < Measure 2")     ; name: "groupTwoGreater"                }
		}

		GroupBox 
		{
			title: qsTr("Plots")
			CheckBox {  text: qsTr("Prior and posterior")           ; name: "plotPriorAndPosterior"                  ; id: plotPriorAndPosterior }
			CheckBox {  text: qsTr("Additional info")               ; name: "plotPriorAndPosteriorAdditionalInfo"    ; indent: true; checked: true; enabled: plotPriorAndPosterior.checked}
			CheckBox {  text: qsTr("Bayes factor robustness check") ; name: "plotBayesFactorRobustness"              ; id: plotBayesFactorRobustness }
			CheckBox {  text: qsTr("Additional info")               ; name: "plotBayesFactorRobustnessAdditionalInfo"; indent: true; checked: true; enabled: plotBayesFactorRobustness.checked}
		}
		
		BayesFactorType { }
    }

    SubjectivePriors { }
}
