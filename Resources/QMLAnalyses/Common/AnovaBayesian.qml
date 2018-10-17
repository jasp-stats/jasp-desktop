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

Form {
    id: form

    TextField { visible: false; name: "plotHeightDescriptivesPlotLegend"; inputType: "integer"; value: "300" }
    TextField { visible: false; name: "plotHeightDescriptivesPlotNoLegend"; inputType: "integer"; value: "300" }
    TextField { visible: false; name: "plotWidthDescriptivesPlotLegend"; inputType: "integer"; value: "430" }
    TextField { visible: false; name: "plotWidthDescriptivesPlotNoLegend"; inputType: "integer"; value: "350" }
    TextField { visible: false; name: "posteriorEstimatesCredibleIntervalInterval"; inputType: "number"; value: "0.950" }
    TextField { visible: false; name: "posteriorEstimatesMCMCIterations"; inputType: "integer"; value: "10000" }

    CheckBox { visible: false; name: "posteriorEstimates";  }
    CheckBox { visible: false; name: "posteriorDistribution";  }

    VariablesForm {
        defaultAssignedVariablesList {
            name: "dependent"
            title: qsTr("Dependent Variable")
            singleItem: true
            allowedColumns: ["scale"]
        }
        AssignedVariablesList {
            name: "fixedFactors"
            title: qsTr("Fixed Factors")
            allowedColumns: ["ordinal", "nominal"]
        }
        AssignedVariablesList {
            name: "randomFactors"
            title: qsTr("Random Factors")
            allowedColumns: ["ordinal", "nominal"]
        }
    }

    GridLayout {
        ButtonGroup {
            title: qsTr("Bayes Factor")
            name: "bayesFactorType"
            RadioButton { text: qsTr("BF\u2081\u2080"); name: "BF10"; checked: true }
            RadioButton { text: qsTr("BF\u2080\u2081"); name: "BF01" }
            RadioButton { text: qsTr("Log(BF\u2081\u2080)"); name: "LogBF10" }
        }

        GroupBox {
            title: qsTr("Output")
            CheckBox { text: qsTr("Effects"); name: "effects"; id: effectsOutput}
            ButtonGroup {
                Layout.leftMargin: 15
                enabled: effectsOutput.checked
                name: "effectsType"
                RadioButton { text: qsTr("Across all models"); name: "allModels"; checked: true}
                RadioButton { text: qsTr("Across matched models"); name: "matchedModels"}
            }
            CheckBox { text: qsTr("Descriptives"); name: "descriptives"}
        }

        ButtonGroup {
            title: qsTr("Order")
            name: "bayesFactorOrder"
            RadioButton { text: qsTr("Compare to null model"); name: "nullModelTop"; checked: true}
            RadioButton { text: qsTr("Compare to best model"); name: "bestModelTop"}
        }
    }

    ExpanderButton {
        text: qsTr("Model")

        VariablesForm {
            height: 200
            showDefaultAssignedVariablesList: false
            listWidth: parent.width * 5 / 9

            availableVariablesList {
                title: qsTr("Components")
                name: "components"
                syncModels: ["fixedFactors", "randomFactors"]
                width: parent.width / 4
            }
            AssignedVariablesList {
                title: qsTr("Model terms")
                name: "modelTerms"
                listViewType: "AssignedAnova"

                ExtraControlColumn {
                    type: "CheckBox"
                    name: "isNuisance"
                    title: "Add to null model"
                }

            }
        }
    }

    ExpanderButton {
        text: qsTr("Post Hoc Tests")

        VariablesForm {
            height: 200
            availableVariablesList {
                name: "postHocTestsAvailable"
                syncModels: "fixedFactors"
            }
            defaultAssignedVariablesList {
                name: "postHocTestsVariables"
            }
        }

        GroupBox {
            title: qsTr("Correction")
            CheckBox { text: qsTr("Null control")   ; name: "postHocTestsNullControl"     ; checked: true }
        }
    }

    ExpanderButton {
        text: qsTr("Descriptives Plots")

        VariablesForm {
            height: 200
            availableVariablesList {        title: qsTr("Factors")          ; name: "descriptivePlotsVariables" ; syncModels: "fixedFactors" }
            defaultAssignedVariablesList {  title: qsTr("Horizontal axis")  ; name: "plotHorizontalAxis"    ; singleItem: true }
            AssignedVariablesList {         title: qsTr("Separate lines")   ; name: "plotSeparateLines"     ; singleItem: true }
            AssignedVariablesList {         title: qsTr("Separate plots")   ; name: "plotSeparatePlots"     ; singleItem: true }
        }

        GroupBox {
            title: qsTr("Display")
            RowLayout {
                CheckBox { text: qsTr("Credible interval"); name: "plotCredibleInterval"; id: plotCredibleInterval }
                PercentField { name: "plotCredibleIntervalInterval"; text: "95"; enabled: plotCredibleInterval.checked}
            }
        }
    }

    ExpanderButton {
        text: qsTr("Additional Options")

        GridLayout {
            GroupBox {
                title: qsTr("Prior")
                TextField { label.text: qsTr("r scale fixed effects"); name: "priorFixedEffects"; inputType: "number"; value: "0.5"; validator: DoubleValidator {bottom: 0; top: 2; decimals: 1} }
                TextField { label.text: qsTr("r scale random effects"); name: "priorRandomEffects"; inputType: "number"; value: "1"; validator: DoubleValidator {bottom: 0; top: 2; decimals: 1} }
            }

            ButtonGroup {
                name: "sampleMode"
                RadioButton { text: qsTr("Auto"); name: "auto"; checked: true}
                RadioButton { text: qsTr("Manual"); name: "manual"; id: samplesManual}
                TextField   {
                    label.text: qsTr("No. samples")
                    name: "fixedSamplesNumber"
                    inputType: "integer"
                    value: "10000"
                    enabled: samplesManual.checked
                    Layout.leftMargin: 15
                    validator: IntValidator {bottom: 0}
                }
            }
        }
    }
}
