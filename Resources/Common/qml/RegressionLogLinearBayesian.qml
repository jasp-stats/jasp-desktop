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

Form {
    id: form
    
    VariablesForm {
        showDefaultAssignedVariablesList: false

        AssignedVariablesList {
            title: qsTr("Counts (optional)")
            name: "counts"
            singleItem: true
        }
        AssignedVariablesList {
            title: qsTr("Factors")
            name: "factors"
            itemType: "fixedFactors"
            allowedColumns: ["ordinal", "nominal"]
        }
    }

    GridLayout {
        columns: 3
        BayesFactorType {}

        GroupBox {
            title: qsTr("Prior")
            DoubleField { text: qsTr("Shape") ; name: "priorShape" ; defaultValue: -1 ; doubleValidator.bottom: -1 }
            DoubleField { text: qsTr("Scale") ; name: "priorScale" ; defaultValue: 0 }
        }

        GroupBox {
            title: qsTr("Model cut-offs")
            IntegerField { text: qsTr("Display best") ; name: "maxModels"; defaultValue: 2; afterLabel.text: qsTr("models"); intValidator.bottom: 2 }
            DoubleField { text: qsTr("Posterior prob.") ; name: "posteriorProbabilityCutOff" ; defaultValue: 0.1 ; doubleValidator.top: 0.5 }
        }
    }

    ExpanderButton {
        title: qsTr("Model")

        VariablesForm {
            height: 200
            listWidth: parent.width * 5 / 9

            availableVariablesList {
                name: "availableTerms"
                title: qsTr("Components")
                width: parent.width / 4
                syncModels: ['factors']
            }
            defaultAssignedVariablesList {
                name: "modelTerms"
                title: qsTr("Model terms")
                listViewType: "Interaction"
            }
        }
    }

    ExpanderButton {
        text: qsTr("Statistics")

        GridLayout {

            GroupBox {
                title: qsTr("Regression Coefficients")
                CheckBox { text: qsTr("Estimates") ; name: "regressionCoefficientsEstimates" }
                CheckBox { text: qsTr("Credible intervals") ; name: "regressionCoefficientsCredibleIntervals" ; id: interval }
                PercentField { text: qsTr("Interval"); name: "regressionCoefficientsCredibleIntervalsInterval" ; defaultValue: 95
                    validator: IntValidator {bottom: 50}
                    enabled: interval.checked
                    indent: true
                }
            }

            GroupBox {
                Row {
                    CheckBox { text: qsTr("Submodel") ; name: "regressionCoefficientsSubmodel"; id: regressionCoefficientsSubmodel }
                    IntegerField { name: "regressionCoefficientsSubmodelNo"; defaultValue: 1 ; intValidator.bottom: 1; enabled: regressionCoefficientsSubmodel.checked }
                }

                GroupBox {
                    indent: true;
                    enabled: regressionCoefficientsSubmodel.checked
                    CheckBox { text: qsTr("Estimates") ; name: "regressionCoefficientsSubmodelEstimates" }
                    CheckBox { text: qsTr("Credible intervals") ; name: "regressionCoefficientsSubmodelCredibleIntervals" ; id: submodelCredibleIntervals }
                    PercentField { text: qsTr("Interval") ; name: "regressionCoefficientsSubmodelCredibleIntervalsInterval"; defaultValue: 95 ; enabled: submodelCredibleIntervals.checked ; indent: true }
                }
            }
        }
    }

    ExpanderButton {
        text: qsTr("Advanced")

        RadioButtonGroup {
            title: qsTr("Samples")
            name: "sampleMode"
            RadioButton { text: qsTr("Auto") ; name: "auto" ; checked: true }
            RadioButton { text: qsTr("Manual") ; name: "manual" ; id: manual }
            IntegerField { text: qsTr("No. samples") ; name: "fixedSamplesNumber" ; defaultValue: 10000 ; fieldWidth: 60 ; enabled: manual.checked ; indent: true }
         }
    }
}
