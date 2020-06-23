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
import JASP				1.0

Section
{
	title: qsTr("Prior")
    property alias informedPriorsEnabled: informedPriors.enabled
	property alias defaultPriorsChecked: defaultPriors.checked

	RadioButtonGroup
	{
		name: "effectSize"
		RadioButton
		{
			label: qsTr("Standardized effect size")
			name: "standardized"
			debug: true
			checked: true
			id: standardized
		}

		RadioButtonGroup
		{
			name: "effectSizeStandardized"
			indent: DEBUG_MODE
			enabled: standardized.checked
			RadioButton
			{
				id: defaultPriors
				label: qsTr("Default"); name: "default"; checked: true
				RadioButtonGroup
				{
					name: "defaultStandardizedEffectSize"
					RadioButton
					{
						label: qsTr("Cauchy"); name: "cauchy"; checked: true; childrenOnSameRow: true
						DoubleField { label: qsTr("scale"); name: "priorWidth"; defaultValue: 0.707; fieldWidth: 50; max: 2; inclusive: JASP.MaxOnly }
					}
				}
			}

			RadioButton
			{
                id: informedPriors
				label: qsTr("Informed"); name: "informative"
				RadioButtonGroup
				{
					name: "informativeStandardizedEffectSize"
					RadioButton
					{
						label: qsTr("Cauchy"); name: "cauchy"; checked: true; childrenOnSameRow: true; id: cauchyInformative
						DoubleField { label: qsTr("location:"); name: "informativeCauchyLocation"; visible: cauchyInformative.checked; defaultValue: 0; min: -3; max: 3 }
						DoubleField { label: qsTr("scale:"); name: "informativeCauchyScale"; visible: cauchyInformative.checked; defaultValue: 0.707; fieldWidth: 50; max: 2; inclusive: JASP.MaxOnly }
					}
					RadioButton
					{
						label: qsTr("Normal"); name: "normal"; childrenOnSameRow: true; id: normalInformative
						DoubleField { label: qsTr("mean:"); name: "informativeNormalMean"; visible: normalInformative.checked; defaultValue: 0; min: -3; max: 3 }
						DoubleField { label: qsTr("std:"); name: "informativeNormalStd"; visible: normalInformative.checked; defaultValue: 0.707; fieldWidth: 50; max: 2 }
					}
					RadioButton
					{
						label: qsTr("t"); name: "t"; childrenOnSameRow: true; id: tInformative
						DoubleField { label: qsTr("location:"); name: "informativeTLocation"; visible: tInformative.checked; defaultValue: 0; min: -3; max: 3 }
						DoubleField { label: qsTr("scale:"); name: "informativeTScale"; visible: tInformative.checked; defaultValue: 0.707; fieldWidth: 50; max: 2; inclusive: JASP.MaxOnly }
						DoubleField { label: qsTr("df:"); name: "informativeTDf"; visible: tInformative.checked; defaultValue: 1; min: 1; max: 100 }
					}
				}
			}
		}

		RadioButton
		{
			label: qsTr("Raw effect size (Dienes)"); name: "dienes"; debug: true
			RadioButtonGroup
			{
				debug: true
				name: "dienesEffectSize"
				RadioButton
				{
					label: qsTr("Uniform"); name: "uniform"; checked: true; childrenOnSameRow: true; id: uniformDienes
					DoubleField { label: qsTr("lower bound:"); name: "uniformDienesLowerBound"; visible: uniformDienes.checked; defaultValue: 0.707; fieldWidth: 50; min: 0; max: 2 }
					DoubleField { label: qsTr("upper bound:"); name: "uniformDienesUpperBound"; visible: uniformDienes.checked; defaultValue: 0.707; fieldWidth: 50; min: 0; max: 2 }
				}
				RadioButton
				{
					label: qsTr("Half-normal"); name: "half_normal"; childrenOnSameRow: true; id: halfNormalDienes
					DoubleField { label: qsTr("std:"); name: "halfNormalDienesStd"; visible: halfNormalDienes.checked; defaultValue: 0.707; fieldWidth: 50; min: 0; max: 2 }
				}
				RadioButton
				{
					label: qsTr("Normal"); name: "normal"; childrenOnSameRow: true; id: normalDienes
					DoubleField { label: qsTr("mean:"); name: "normalDienesMean"; visible: normalDienes.checked; defaultValue: 0.707; fieldWidth: 50; min: 0; max: 2 }
					DoubleField { label: qsTr("std:"); name: "normalDienesStd"; visible: normalDienes.checked; defaultValue: 0.707; fieldWidth: 50; min: 0; max: 2 }
				}
			}
		}
    }

}
