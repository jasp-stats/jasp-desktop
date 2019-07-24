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
import QtQuick          2.8
import QtQuick.Layouts  1.3
import JASP.Controls    1.0
import JASP.Theme		1.0


Form
{
    VariablesForm
    {
        AvailableVariablesList
        {
            name: "availableVariables"
        }
        AssignedVariablesList
        {
            title: "Predictors"
            name:  "predictor"
            allowedColumns: []
        }
        AssignedVariablesList
        {
            title: "Mediators"
            name:  "mediators"
            allowedColumns: ["scale", "ordinal"]
        }
        AssignedVariablesList
        {
            title: "Outcome"
            name:  "dependent"
            allowedColumns: ["scale", "ordinal"]
        }
        AssignedVariablesList
        {
            title: "Background confounders"
            name:  "confounds"
            allowedColumns: []
        }
    }


	Section
	{
        title: qsTr("Options")
        ColumnLayout {
            GroupBox
            {
                CheckBox { label: qsTr("Standardized estimates") ; name: "std" }
                CheckBox { label: qsTr("Lavaan syntax")     ; name: "showSyntax" }
                CheckBox { label: qsTr("R-squared")         ; name: "rsquared" }
            }
            GroupBox
            {
                title: qsTr("Additional parameter estimates")
                CheckBox { label: qsTr("Total indirect effects");  name: "showtotind"; checked: true }
                CheckBox { label: qsTr("Residual covariances");    name: "showres";    checked: true }
            }
        }
        GroupBox
        {
            CIField {
                text: qsTr("Confidence intervals")
                name: "ciWidth"
            }
            RadioButtonGroup {
                title: qsTr("Method")
                name: "se"
                RadioButton { text: qsTr("Standard")  ; name: "standard" ; checked: true }
                RadioButton { text: qsTr("Robust")    ; name: "robust" }
                RadioButton {
                    text: qsTr("Bootstrap")
                    name: "bootstrap"
                    IntegerField {
                        text: qsTr("Replications")
                        name: "bootstrapNumber"
                        defaultValue: 1000
                        min: 500
                        max: 100000
                    }
                    DropDown {
                        label: qsTr("Type")
                        name: "bootCItype"
                        values: [
                            { label: qsTr("Bias-corrected percentile"), value: "bca.simple"   },
                            { label: qsTr("Percentile"),                value: "perc"         },
                            { label: qsTr("Normal theory"),             value: "norm"         }
                        ]
                    }
                }
            }
        }
    }

	Section {
        text: qsTr("Plots")
        CheckBox { 
            text:   qsTr("Model plot")
            name:   "pathplot"
            id:     pathPlot 
            CheckBox { text: qsTr("Parameter estimates") ; name: "plotpars" }
            CheckBox { text: qsTr("Legend") ; name: "plotlegend" }
        }
    }
    
	Section {
        text: qsTr("Advanced")
        GroupBox {
            Layout.fillWidth: true
            RadioButtonGroup {
                title: qsTr("Missing value handling")
                name: "missing"
                RadioButton { text: qsTr("Full Information Maximum Likelihood") ; name: "fiml" ; checked: true }
                RadioButton { text: qsTr("Exclude cases listwise")              ; name: "listwise"             }
            }
            RadioButtonGroup {
                title: qsTr("Emulation")
                name: "mimic"
                RadioButton { text: qsTr("None")  ; name: "lavaan"  ; checked: true }
                RadioButton { text: qsTr("Mplus") ; name: "Mplus" }
                RadioButton { text: qsTr("EQS")   ; name: "EQS"   }
            }
        }
        GroupBox {
            Layout.fillWidth: true
            RadioButtonGroup {
                title: qsTr("Estimator")
                name: "estimator"
                RadioButton { text: qsTr("Auto") ; name: "default"; checked: true }
                RadioButton { text: qsTr("ML")   ; name: "ML"       }
                RadioButton { text: qsTr("GLS")  ; name: "GLS"      }
                RadioButton { text: qsTr("WLS")  ; name: "WLS"      }
                RadioButton { text: qsTr("ULS")  ; name: "ULS"      }
                RadioButton { text: qsTr("DWLS") ; name: "DWLS"     }
            }
        }
    }
    
}


