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

	IntegerField { name: "sampleSize"; text: qsTr("Sample size"); intValidator.bottom: 2 }

    Divider { }

	RadioButtonGroup
	{
		name: "correlationCoefficient"
		title: qsTr("Correlation Coefficient")
		Layout.columnSpan: 2
		RadioButton
		{
			value: "pearsonRho"; text: qsTr("Pearson's rho"); childrenOnSameRow: true
			DoubleField { name: "pearsonRhoValue"; defaultValue: 0; min: -1; max: 1 }
		}
		RadioButton
		{
			value: "kendallTau"; text: qsTr("Kendall's tau-b"); childrenOnSameRow: true
			DoubleField { name: "kendallTauValue"; defaultValue: 0; min: -1; max: 1 }
		}
	}

	RadioButtonGroup
	{
		title: qsTr("Hypothesis")
		name: "hypothesis"
		RadioButton { value: "correlated";				text: qsTr("Correlated"); checked: true	}
		RadioButton { value: "correlatedPositively";	text: qsTr("Correlated positively")		}
		RadioButton { value: "correlatedNegatively";	text: qsTr("Correlated negatively")		}
	}

	GroupBox
	{
		title: qsTr("Plots")
		CheckBox
		{
			name: "plotPriorAndPosterior";				text: qsTr("Prior and posterior")
			CheckBox { name: "plotPriorAndPosteriorAdditionalInfo";		text: qsTr("Additional info"); checked: true }
		}
		CheckBox
		{
			name: "plotBayesFactorRobustness";			text: qsTr("Bayes factor robustness check")
			CheckBox { name: "plotBayesFactorRobustnessAdditionalInfo";	text: qsTr("Additional info"); checked: true }
		}
	}

	BayesFactorType { }

	GroupBox
	{
		title: qsTr("Prior")
		DoubleField { name: "priorWidth"; text: qsTr("Stretched beta prior width"); defaultValue: 1; min: 0; max: 2 }
	}
}
