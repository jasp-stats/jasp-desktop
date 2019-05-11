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

	IntegerField { label: qsTr("Sample size"); name: "sampleSize" ; min: 3; Layout.columnSpan: 2; defaultValue: 3 }

	Group
	{
		title: qsTr("Null Model")
		IntegerField {	label: qsTr("Number of covariates"); name: "numberOfCovariatesNull" }
		DoubleField {	label: qsTr("R-squared");			name: "unadjustedRSquaredNull" ; max: 0.9999 }
	}

	Group
	{
		title: qsTr("Alternative Model")
		IntegerField {	label: qsTr("Number of covariates"); name: "numberOfCovariatesAlternative" ; min: 1; defaultValue: 1 }
		DoubleField {	label: qsTr("R-squared");			name: "unadjustedRSquaredAlternative" ; max: 0.9999 }
	}

	Divider { }

	BayesFactorType { }

	Group
	{
		title: qsTr("Plots")
		CheckBox
		{
			name: "plotBayesFactorRobustness"; label: qsTr("Bayes factor robustness check")
			CheckBox { name: "plotBayesFactorRobustnessAdditionalInfo"; label: qsTr("Additional info"); checked: true }
		}
	}

    Section 
	{
        title: qsTr("Advanced Options")

        Group
		{
            title: qsTr("Prior")
			DoubleField { label: qsTr("r scale covariates"); defaultValue: 0.3536 ; name: "priorWidth" ; fieldWidth: 80; max: 2 }
        }
    }
}
