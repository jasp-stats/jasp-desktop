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

import QtQuick			2.8
import QtQuick.Layouts	1.3
import JASP.Controls	1.0
import JASP.Widgets		1.0
import JASP				1.0


Form 
{

	Group
	{
		IntegerField { name: "successes";	label: qsTr("Successes")	}
		IntegerField { name: "failures";	label: qsTr("Failures")		}
		FormulaField { name: "testValue";	label: qsTr("Test value"); defaultValue: "0.5" ; max: 1 }
    }

    Divider { }

	RadioButtonGroup
	{
		title: qsTr("Alt. Hypothesis")
		name: "hypothesis"
		RadioButton { value: "notEqualToTestValue";		label: qsTr("\u2260 Test value"); checked: true	}
		RadioButton { value: "greaterThanTestValue";	label: qsTr("> Test value")						}
		RadioButton { value: "lessThanTestValue";		label: qsTr("< Test value")						}
	}

	Group
	{
		title: qsTr("Plots")
		CheckBox
		{
			name: "plotPriorAndPosterior";		label: qsTr("Prior and posterior")
			CheckBox { name: "plotPriorAndPosteriorAdditionalInfo"; label: qsTr("Additional info"); checked: true }
		}
	}

	BayesFactorType { }


	Group
	{
		title: qsTr("Prior")
		DoubleField { name: "betaPriorParamA"; label: qsTr("Beta prior: parameter a"); defaultValue: 1; max: 10000; inclusive: JASP.None; decimals: 3 }
		DoubleField { name: "betaPriorParamB"; label: qsTr("Beta prior: parameter b"); defaultValue: 1; max: 10000; inclusive: JASP.None; decimals: 3 }
	}
}
