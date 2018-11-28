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


ExpanderButton {
    text: qsTr("Prior")

    ButtonGroup {
        name: "effectSize"
        RadioButton {text: qsTr("Standardized effect size"); name: "standardized"; checked: true; debug: true; id: standardized}
        ButtonGroup {
            Layout.leftMargin: DEBUG_MODE ? 25 : 0
            enabled: standardized.checked
            name: "effectSizeStandardized"
            RadioButton {text: qsTr("Default"); name: "default"; checked: true; id: defaultEffect}
            ButtonGroup {
                Layout.leftMargin: 25
                enabled: defaultEffect.checked
                name: "defaultStandardizedEffectSize"
                Row {
                    RadioButton {text: qsTr("Cauchy"); name: "cauchy"; checked: true}
                    TextField {label.text: qsTr("scale"); name: "priorWidth"; inputType: "number"; text: "0.707"; validator: DoubleValidator {bottom: 0; top: 2}}
                }
            }

            RadioButton {text: qsTr("Informed"); name: "informative"; id: informative}
            ButtonGroup {
                enabled: informative.checked
                Layout.leftMargin: 25
                name: "informativeStandardizedEffectSize"
                Row {
                    spacing: 10
                    RadioButton { text: qsTr("Cauchy")  ; name: "cauchy"; checked: true; id: cauchyInformative}
                    TextField { label.text: qsTr("location:"); name: "informativeCauchyLocation"; visible: cauchyInformative.checked; inputType: "number"; text: "0"; validator: DoubleValidator {bottom: -3; top: -3}}
                    TextField { label.text: qsTr("scale:"); name: "informativeCauchyScale"; visible: cauchyInformative.checked; inputType: "number"; text: "0.707"; validator: DoubleValidator {bottom: 0; top: 2}}
                }
                Row {
                    spacing: 10
                    RadioButton { text: qsTr("Normal")  ; name: "normal"; id: normalInformative}
                    TextField { label.text: qsTr("mean:"); name: "informativeNormalMean"; visible: normalInformative.checked; inputType: "number"; text: "0"; validator: DoubleValidator {bottom: -3; top: -3}}
                    TextField { label.text: qsTr("std:"); name: "informativeNormalStd"; visible: normalInformative.checked; inputType: "number"; text: "0.707"; validator: DoubleValidator {bottom: 0; top: 2}}
                }
                Row {
                    spacing: 10
                    RadioButton { text: qsTr("t")       ; name: "t"; id: tInformative}
                    TextField { label.text: qsTr("location:"); name: "informativeTLocation"; visible: tInformative.checked; inputType: "number"; text: "0"; validator: DoubleValidator {bottom: -3; top: -3}}
                    TextField { label.text: qsTr("scale:"); name: "informativeTScale"; visible: tInformative.checked; inputType: "number"; text: "0.707"; validator: DoubleValidator {bottom: 0.001; top: 2}}
                    TextField { label.text: qsTr("df:"); name: "informativeTDf"; visible: tInformative.checked; inputType: "number"; text: "1"; validator: DoubleValidator {bottom: 1; top: 100}}
                }
            }
        }

        RadioButton {text: qsTr("Raw effect size (Dienes)"); name: "dienes"; debug: true; id: dienes}
        ButtonGroup {
            enabled: dienes.checked
            Layout.leftMargin: 25
            debug: true
            name: "dienesEffectSize"
            Row {
                spacing: 10
                RadioButton { text: qsTr("Uniform"); name: "uniform"; checked: true; id: uniformDienes}
                TextField { label.text: qsTr("lower bound:"); name: "uniformDienesLowerBound"; visible: uniformDienes.checked; inputType: "number"; text: "0.707"; validator: DoubleValidator {bottom: 0; top: 2}}
                TextField { label.text: qsTr("upper bound:"); name: "uniformDienesUpperBound"; visible: uniformDienes.checked; inputType: "number"; text: "0.707"; validator: DoubleValidator {bottom: 0; top: 2}}
            }
            Row {
                spacing: 10
                RadioButton { text: qsTr("Half-normal"); name: "half_normal"; id: halfNormalDienes}
                TextField { label.text: qsTr("std:"); name: "halfNormalDienesStd"; visible: halfNormalDienes.checked; inputType: "number"; text: "0.707"; validator: DoubleValidator {bottom: 0; top: 2}}
            }
            Row {
                spacing: 10
                RadioButton { text: qsTr("Normal"); name: "normal"; id: normalDienes}
                TextField { label.text: qsTr("mean:"); name: "normalDienesMean"; visible: normalDienes.checked; inputType: "number"; text: "0.707"; validator: DoubleValidator {bottom: 0; top: 2}}
                TextField { label.text: qsTr("std:"); name: "normalDienesStd"; visible: normalDienes.checked; inputType: "number"; text: "0.707"; validator: DoubleValidator {bottom: 0; top: 2}}
            }
        }
    }

}
