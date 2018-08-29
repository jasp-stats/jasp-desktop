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
    
    VariablesForm {
        defaultAssignedVariablesList {
            title: qsTr("Variables")
            allowedColumns: ["ordinal", "nominal"]            
        }
    }

    GridLayout {
        GroupBox {
            title: qsTr("Scale Statistics")
            CheckBox {  text: qsTr("McDonald's ω")                      ; name: "mcDonaldScale"; checked: true }
            CheckBox {  text: qsTr("Cronbach's α")                      ; name: "alphaScale"; id: alphaScale }
            ButtonGroup {
                Layout.leftMargin: 10
                enabled: alphaScale.checked
                name: "alphaScaleStandardized"
                RadioButton {   text: qsTr("Unstandardized")            ; name: "_1unstandardized"; checked: true   }
                RadioButton {   text: qsTr("Standardized")              ; name: "_2standardized"                    }
            }
            CheckBox {  text: qsTr("Gutmann's λ6")                      ; name: "gutmannScale"    }            
            CheckBox {  text: qsTr("Greatest lower bound")              ; name: "glbScale"        }
            CheckBox {  text: qsTr("Average interitem correlation")     ; name: "averageInterItemCor"}
            CheckBox {  text: qsTr("Mean")                              ; name: "meanScale"       }
            CheckBox {  text: qsTr("Standard deviation")                ;  name: "sdScale"        }
        }

        GroupBox {
            title: qsTr("Individual Item Statistics")
            CheckBox {  text: qsTr("McDonald's ω  (if item dropped)")   ; name: "mcDonaldItem"    }
            CheckBox {  text: qsTr("Cronbach's α (if item dropped)")    ; name: "alphaItem"       }
            CheckBox {  text: qsTr("Gutmann's λ6 (if item dropped)")    ; name: "gutmannItem"     }
            CheckBox {  text: qsTr("Mean")                              ; name: "meanItem"        }
            CheckBox {  text: qsTr("Standard deviation")                ; name: "sdItem"          }
            CheckBox {  text: qsTr("Item-rest correlation")             ; name: "itemRestCor"     }
        }
    }

    ExpanderButton {
        text: qsTr("Reverse-Scaled Items")

        VariablesForm {
            formHeight: 150
            availableVariablesList {
                title: qsTr("Normal-Scaled Items")
                name: "normalScaledItems"
                syncModels: "variables"
            }
            defaultAssignedVariablesList {
                title: qsTr("Reverse-Scaled Items")
                name: "reverseScaledItems"
            }
        }
    }

    ExpanderButton {
        text: qsTr("Advanced Options")

        GridLayout {
            ButtonGroup {
                title: qsTr("Missing Values")
                name: "missingValues"
                RadioButton { text: qsTr("Exclude cases listwise")    ; name: "excludeCasesListwise"    ; checked: true }
                RadioButton { text: qsTr("Exclude cases pairwise")    ; name: "excludeCasesPairwise" }
            }            
            
            GroupBox {
                title: qsTr("Confidence Interval")
                CheckBox {id: confAlpha; text: qsTr("Cronbach's α analytical")  ; name: "confAlpha" }    
                PercentField {Layout.leftMargin: 25; name: "confAlphaLevel"; defaultValue: 95; label.text: qsTr("Confidence"); with1Decimal: true; enabled: confAlpha.checked }
            }            
        }

    }
}
