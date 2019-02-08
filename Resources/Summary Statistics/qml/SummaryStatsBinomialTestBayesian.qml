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
		IntegerField { name: "successes";	text: qsTr("Successes")	}
		IntegerField { name: "failures";	text: qsTr("Failures")	}
		DoubleField  { name: "testValue";	text: qsTr("Test value"); defaultValue: 0.5 ; max: 1 }
    }

    Divider { }

	RadioButtonGroup
	{
		title: qsTr("Hypothesis")
		name: "hypothesis"
		RadioButton { value: "notEqualToTestValue";		text: qsTr("\u2260 Test value"); checked: true	}
		RadioButton { value: "greaterThanTestValue";	text: qsTr("> Test value")						}
		RadioButton { value: "lessThanTestValue";		text: qsTr("< Test value")						}
	}

	Group
	{
		title: qsTr("Plots")
		CheckBox
		{
			name: "plotPriorAndPosterior";		text: qsTr("Prior and posterior")
			CheckBox { name: "plotPriorAndPosteriorAdditionalInfo"; text: qsTr("Additional info"); checked: true }
		}
	}

	BayesFactorType { }


	Group
	{
		title: qsTr("Prior")
		DoubleField { name: "betaPriorParamA"; text: qsTr("Beta prior: parameter a"); defaultValue: 1 }
		DoubleField { name: "betaPriorParamB"; text: qsTr("Beta prior: parameter b"); defaultValue: 1 }
	}
}
