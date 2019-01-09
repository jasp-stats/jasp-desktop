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

	IntegerField { text: qsTr("Sample size"); name: "sampleSize" ; intValidator.bottom: 2 }

    Divider { }

    GridLayout 
	{
		ButtonGroup 
		{
			title: qsTr("Correlation Coefficient") ; name: "correlationCoefficient"
			Layout.columnSpan: 2
			Row 
			{
				RadioButton { text: qsTr("Pearson's rho") ; name: "pearsonRho"; id: pearsonRho; checked: true}
				DoubleField   { name: "pearsonRhoValue"; defaultValue: 0; visible: pearsonRho.checked; doubleValidator { bottom: -1; top: 1 } }
			}
			Row 
			{
				RadioButton { text: qsTr("Kendall's tau-b") ; name: "kendallTau"; id: kendallTau }
				DoubleField   { name: "kendallTauValue"; defaultValue: 0; visible: kendallTau.checked; doubleValidator { bottom: -1; top: 1 } }
			}
		}
		
		ButtonGroup 
		{
			title: qsTr("Hypothesis")
			name: "hypothesis"
			RadioButton { text: qsTr("Correlated")            ; name: "correlated" ; checked: true }
			RadioButton { text: qsTr("Correlated positively") ; name: "correlatedPositively"       }
			RadioButton { text: qsTr("Correlated negatively") ; name: "correlatedNegatively"       }
		}

		GroupBox 
		{
			title: qsTr("Plots")
			CheckBox { text: qsTr("Prior and posterior")           ; name: "plotPriorAndPosterior"                  ; id: plotPriorAndPosterior }
			CheckBox { text: qsTr("Additional info")               ; name: "plotPriorAndPosteriorAdditionalInfo"    ; indent: true; checked: true; enabled: plotPriorAndPosterior.checked}
			CheckBox { text: qsTr("Bayes factor robustness check") ; name: "plotBayesFactorRobustness"              ; id: plotBayesFactorRobustness }
			CheckBox { text: qsTr("Additional info")               ; name: "plotBayesFactorRobustnessAdditionalInfo"; indent: true; checked: true; enabled: plotBayesFactorRobustness.checked}
		}
		
        BayesFactorType { }

		GroupBox 
		{
			title: qsTr("Prior")
			DoubleField { text: qsTr("Stretched beta prior width"); defaultValue: 1 ; name: "priorWidth" ; doubleValidator {bottom: 0; top: 2} }
		}
    }
}
