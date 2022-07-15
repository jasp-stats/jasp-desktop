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
import JASP.Controls 1.0


RadioButtonGroup
{
    title: qsTr("Bayes Factor")                     ; name: "bayesFactorType"

	RadioButton { label: qsTr("BF\u2081\u2080")      ; name: "BF10"; checked: true; info: qsTr("Bayes factor to quantify evidence for the alternative hypothesis relative to the null hypothesis.") }
	RadioButton { label: qsTr("BF\u2080\u2081")      ; name: "BF01"               ; info: qsTr("Bayes factor to quantify evidence for the null hypothesis relative to the alternative hypothesis.") }
	RadioButton { label: qsTr("Log(BF\u2081\u2080)") ; name: "LogBF10"            ; info: qsTr("Natural logarithm of BF10.") }
}
