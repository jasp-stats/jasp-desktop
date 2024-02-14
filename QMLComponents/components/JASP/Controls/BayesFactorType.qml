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

	property string correlated:	"twoSided"

	property string _sub0		: "\u2080"
	property string _sub1		: "\u2081"
	property string _subMin		: "\u208B"
	property string _subPlus	: "\u208A"
	property string _subSign	: correlated == "twoSided" ? _sub1 : (correlated == "greater" ? _subPlus : _subMin)

	RadioButton { label: "BF" + _subSign + _sub0			; name: "BF10"; checked: true; info: qsTr("Bayes factor to quantify evidence for the alternative hypothesis relative to the null hypothesis.") }
	RadioButton { label: "BF" + _sub0 + _subSign			; name: "BF01"               ; info: qsTr("Bayes factor to quantify evidence for the null hypothesis relative to the alternative hypothesis.") }
	RadioButton { label: "Log(BF" + _subSign + _sub0 + ")"	; name: "LogBF10"            ; info: qsTr("Natural logarithm of BF10.") }
}
