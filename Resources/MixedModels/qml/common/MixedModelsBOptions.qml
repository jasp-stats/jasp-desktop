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
import QtQuick			2.12
import JASP.Controls	1.0
import JASP.Widgets		1.0
import JASP				1.0
import QtQuick.Layouts	1.3

Section
{
	title: qsTr("Options")

	Group
	{

		IntegerField
		{
			name:			"warmup"
			id:				warmup
			label:			qsTr("Warmup")
			defaultValue:	2000
			min:			1
		}

		IntegerField
		{
			name:			"iteration"
			label:			qsTr("Iterations")
			defaultValue:	4000
			min:			warmup.value
		}

		IntegerField
		{
			name:			"chains"
			label:			qsTr("Chains")
			defaultValue:	3
			min:			1
		}

		DoubleField
		{
			name:			"adapt_delta"
			label:			qsTr("Adapt delta")
			defaultValue:	0.80
			min:			0
			max:			1
		}

		IntegerField
		{
			name:			"max_treedepth"
			label:			qsTr("Maximum treedepth")
			defaultValue:	10
			min:			1
		}
	}

	Group
	{

		RadioButtonGroup
		{
			name:		"show"
			title:		qsTr("Show")
			RadioButton { value: "deviation";	label: qsTr("Deviations from mean"); checked: true}
			RadioButton { value: "mmeans";		label: qsTr("Marginal means") }
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
	}

	SetSeed{}

	CIField
	{
		name:	"summaryCI"
		label:	qsTr("Credible interval")
	}
}
