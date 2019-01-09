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

	IntegerField { text: qsTr("Sample size"); name: "sampleSize" ; intValidator.bottom: 3 }

    GridLayout 
	{
		GroupBox 
		{
			title: qsTr("Null model")
			IntegerField {	text: qsTr("Number of covariates"); name: "numberOfCovariatesNull" }
			DoubleField {	text: qsTr("R-squared");			name: "unadjustedRSquaredNull" ; doubleValidator.top: 0.9999 }
        }
		
		GroupBox 
		{
			title: qsTr("Alternative model")
			IntegerField {	text: qsTr("Number of covariates"); name: "numberOfCovariatesAlternative" ; intValidator.bottom: 1 }
			DoubleField {	text: qsTr("R-squared");			name: "unadjustedRSquaredAlternative" ; doubleValidator.top: 0.9999 }
		}
    }

    Divider { }

    GridLayout 
	{
		BayesFactorType { }
 
		GroupBox 
		{
			title: qsTr("Plots")
			CheckBox { text: qsTr("Bayes factor robustness check") ; name: "plotBayesFactorRobustness"              ; id: plotBayesFactorRobustness }
			CheckBox { text: qsTr("Additional info")               ; name: "plotBayesFactorRobustnessAdditionalInfo"; indent: true; checked: true; enabled: plotBayesFactorRobustness.checked}
		}
    }

    ExpanderButton 
	{
        text: qsTr("Advanced Options")

        GroupBox 
		{
            title: qsTr("Prior")
            DoubleField { text: qsTr("r scale covariates"); defaultValue: 0.3536 ; name: "priorWidth" ; fieldWidth: 60; doubleValidator.top: 2 }
        }
    }
}
