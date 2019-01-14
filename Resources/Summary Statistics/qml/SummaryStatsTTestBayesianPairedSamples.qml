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
			name: "hypothesis"
			title: qsTr("Hypothesis")
			RadioButton { value: "groupsNotEqual";	text: qsTr("Measure 1 \u2260 Measure 2"); checked: true }
			RadioButton { value: "groupOneGreater";	text: qsTr("Measure 1 > Measure 2")						}
			RadioButton { value: "groupTwoGreater";	text: qsTr("Measure 1 < Measure 2")						}
		}

		GroupBox 
		{
			title: qsTr("Plots")
			CheckBox { name: "plotPriorAndPosterior";					text: qsTr("Prior and posterior"); id: plotPriorAndPosterior }
			CheckBox { name: "plotPriorAndPosteriorAdditionalInfo";		text: qsTr("Additional info"); indent: true; checked: true; enabled: plotPriorAndPosterior.checked }
			CheckBox { name: "plotBayesFactorRobustness";				text: qsTr("Bayes factor robustness check"); id: plotBayesFactorRobustness }
			CheckBox { name: "plotBayesFactorRobustnessAdditionalInfo";	text: qsTr("Additional info"); indent: true; checked: true; enabled: plotBayesFactorRobustness.checked }
		}
		
		BayesFactorType { }
    }

    SubjectivePriors { }
}
