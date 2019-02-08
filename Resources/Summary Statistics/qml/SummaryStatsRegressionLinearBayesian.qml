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

	IntegerField { text: qsTr("Sample size"); name: "sampleSize" ; min: 3; Layout.columnSpan: 2 }

	Group
	{
		title: qsTr("Null model")
		IntegerField {	text: qsTr("Number of covariates"); name: "numberOfCovariatesNull" }
		DoubleField {	text: qsTr("R-squared");			name: "unadjustedRSquaredNull" ; max: 0.9999 }
	}

	Group
	{
		title: qsTr("Alternative model")
		IntegerField {	text: qsTr("Number of covariates"); name: "numberOfCovariatesAlternative" ; min: 1 }
		DoubleField {	text: qsTr("R-squared");			name: "unadjustedRSquaredAlternative" ; max: 0.9999 }
	}

	Divider { }

	BayesFactorType { }

	GroupBox
	{
		title: qsTr("Plots")
		CheckBox
		{
			name: "plotBayesFactorRobustness"; text: qsTr("Bayes factor robustness check")
			CheckBox { name: "plotBayesFactorRobustnessAdditionalInfo"; text: qsTr("Additional info"); checked: true }
		}
	}

    ExpanderButton 
	{
        title: qsTr("Advanced Options")

        GroupBox 
		{
            title: qsTr("Prior")
			DoubleField { text: qsTr("r scale covariates"); defaultValue: 0.3536 ; name: "priorWidth" ; fieldWidth: 80; max: 2 }
        }
    }
}
