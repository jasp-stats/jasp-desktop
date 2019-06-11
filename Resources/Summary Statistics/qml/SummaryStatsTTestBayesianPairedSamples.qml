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

	Group
	{
		DoubleField  { label: qsTr("t");			name: "tStatistic"; negativeValues: true	}
		IntegerField { label: qsTr("Group size");	name: "n1Size"								}
	}
	
    Divider { }

	RadioButtonGroup
	{
		name: "hypothesis"
		title: qsTr("Alt. Hypothesis")
		RadioButton { value: "groupsNotEqual";	label: qsTr("Measure 1 \u2260 Measure 2"); checked: true	}
		RadioButton { value: "groupOneGreater";	label: qsTr("Measure 1 > Measure 2")						}
		RadioButton { value: "groupTwoGreater";	label: qsTr("Measure 1 < Measure 2")						}
	}

	Group
	{
		title: qsTr("Plots")
		CheckBox
		{
			name: "plotPriorAndPosterior";		label: qsTr("Prior and posterior")
			CheckBox { name: "plotPriorAndPosteriorAdditionalInfo";		label: qsTr("Additional info"); checked: true }
		}
		CheckBox
		{
			name: "plotBayesFactorRobustness";	label: qsTr("Bayes factor robustness check")
			CheckBox { name: "plotBayesFactorRobustnessAdditionalInfo";	label: qsTr("Additional info"); checked: true }
		}
	}

	BayesFactorType { }

    SubjectivePriors { }
}
