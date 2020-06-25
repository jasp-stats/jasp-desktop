//
// Copyright (C) 2013-2020 University of Amsterdam
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
import QtQuick			2.12
import JASP.Controls	1.0
import JASP.Widgets		1.0
import JASP				1.0
import QtQuick.Layouts	1.3


Section
{
	title: qsTr("Options")

	property bool allMethodOptions: true


	RadioButtonGroup
	{
		columns:				2
		name:					"type"
		title:					qsTr("Type")
		radioButtonsOnSameRow:	true
		RadioButton { value: "2"; label: qsTr("II") }
		RadioButton { value: "3"; label: qsTr("III"); checked: true }
	}

	CheckBox
	{
		enabled:	method.currentValue == "PB" | method.currentValue == "LRT"
		name:		"test_intercept"
		label:		qsTr("Test intercept")
	}

	Group
	{
		DropDown
		{
			name:	"method"
			label:	qsTr("Test model terms")
			id:		method
			values: allMethodOptions ?
			[
				{ label: "Satterthwaite",					value: "S"},
				{ label: "Kenward-Roger",					value: "KR"},
				{ label: qsTr("Likelihood ratio tests"),	value: "LRT"},
				{ label: qsTr("Parametric bootstrap"),		value: "PB"}
			] :
			[
				{ label: qsTr("Likelihood ratio tests"),	value: "LRT"},
				{ label: qsTr("Parametric bootstrap"),		value: "PB"}
			]
		}

		IntegerField
		{
			enabled:		method.currentValue == "PB"
			name:			"bootstrap_samples"
			label:			qsTr("No. samples")
			defaultValue:	500
			min:			100
			fieldWidth:		60
		}
	}

	Group
	{
		CheckBox
		{
			name:	"fitStats"
			label:	qsTr("Model summary")
		}

		CheckBox
		{
			name:	"showFE"
			label:	qsTr("Fixed effects estimates")
		}

		CheckBox
		{
			name:	"showRE"
			label:	qsTr("Variance/correlation estimates")
		}
	}

	SetSeed{}

	CheckBox
	{
		name:	"pvalVS"
		label:	qsTr("Vovk-Sellke maximum p-ratio")
	}

}
