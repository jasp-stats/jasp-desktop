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
import "qrc:/JASPControls"

Form {
    id: form

    TextField { visible: false; name: "plotWidthQQPlot"; inputType: "integer"; text: "300" }
    TextField { visible: false; name: "plotHeightQQPlot"; inputType: "integer"; text: "300" }
    TextField { visible: false; name: "plotHeightDescriptivesPlotLegend"; inputType: "integer"; text: "300" }
    TextField { visible: false; name: "plotHeightDescriptivesPlotNoLegend"; inputType: "integer"; text: "300" }
    TextField { visible: false; name: "plotWidthDescriptivesPlotLegend"; inputType: "integer"; text: "430" }
    TextField { visible: false; name: "plotWidthDescriptivesPlotNoLegend"; inputType: "integer"; text: "350" }

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
            debug: true
        }
        AssignedVariablesList {
            name: "wlsWeights"
            title: qsTr("WLS Weights")
            singleItem: true
            allowedColumns: ["scale"]
        }
    }


    ExpanderButton {
        text: qsTr("Model")

        VariablesForm {
            formHeight: 200
            availableVariablesList {
                title: qsTr("Components")
                name: "components"
                syncModels: ["fixedFactors", "randomFactors"]
            }
            defaultAssignedVariablesList {
                title: qsTr("Model terms")
                name: "modelTerms"
                listViewType: "assignedAnova"
            }
        }
        
        TextField { name: "sumOfSquares"; label.text: qsTr("Sum of squares"); text: "type3"; enabled: false}

    }

    ExpanderButton {
        text: qsTr("Assumption Checks")

        CheckBox { text: qsTr("Homogeneity tests")      ; name: "homogeneityTests"}
        CheckBox { text: qsTr("Q-Q plot of residuals")  ; name: "qqPlot"}
    }
    
    ExpanderButton {
        text: qsTr("Contrasts")

        TableView {
            title: qsTr("Factors")
            syncModels: "fixedFactors"
            name: "contrasts"
            comboModel: ["none", "deviation", "simple", "difference", "Helmert", "repeated", "polynomial"]
            columns: ["ComboBox"]
        }
    }

    ExpanderButton {
        text: qsTr("Post Hoc Tests")
        
        VariablesForm {
            formHeight: 200
            availableVariablesList {
                name: "postHocTestsAvailable"
                syncModels: "fixedFactors"
            }
            defaultAssignedVariablesList {
                name: "postHocTestsVariables"
            }
        }
        
        CheckBox { text: qsTr("Effect Size")        ; name: "postHocTestEffectSize"}
        
        GroupBox {
            title: qsTr("Correction")
            CheckBox { text: qsTr("Tukey")          ; name: "postHocTestsTukey"     ; checked: true }
            CheckBox { text: qsTr("Scheffe")        ; name: "postHocTestsScheffe"                   }
            CheckBox { text: qsTr("Bonferroni")     ; name: "postHocTestsBonferroni"                }
            CheckBox { text: qsTr("Holm")           ; name: "postHocTestsHolm"                      }
        }
    }

    ExpanderButton {
        text: qsTr("Descriptives Plots")

        VariablesForm {
            formHeight: 200
            availableVariablesList {        title: qsTr("Factors")          ; name: "descriptivePlotsVariables" ; syncModels: "fixedFactors" }
            defaultAssignedVariablesList {  title: qsTr("Horizontal axis")  ; name: "plotHorizontalAxis"    ; singleItem: true }
            AssignedVariablesList {         title: qsTr("Separate lines")   ; name: "plotSeparateLines"     ; singleItem: true }
            AssignedVariablesList {         title: qsTr("Separate plots")   ; name: "plotSeparatePlots"     ; singleItem: true }
        }
        
        GroupBox {
            title: qsTr("Display")
            CheckBox { text: qsTr("Display error bars"); name: "plotErrorBars" }
            
           ButtonGroup {
               name: "errorBarType"
               RadioButton { text: qsTr("Confidence Interval"); name: "confidenceInterval"; checked: true; id: confidenceInterval }
               PercentField {Layout.leftMargin: 15; label.text: qsTr("Interval"); name: "confidenceIntervalInterval"; defaultValue: 95; enabled: confidenceInterval.checked}
               RadioButton { text: qsTr("Standard error"); name: "standardError" }
           }
        }
    }

    ExpanderButton {
        text: qsTr("Additional Options")
        Label { text: qsTr("Marginal means") }
            
        VariablesForm {
            formHeight: 200
            availableVariablesList {        name: "marginalMeansTermsAvailable" ; syncModels: "modelTerms"; showVariableIcon: false }
            defaultAssignedVariablesList {  name: "marginalMeansTerms"; showVariableIcon: false }
        }
        
        CheckBox { text: qsTr("Compare marginal means to 0")    ; name: "marginalMeansCompareMainEffects" }
        TextField { Layout.leftMargin: 15; name: "marginalMeansCIAdjustment"; label.text: qsTr("Confidence interval adjustment"); text: "none"; enabled: false}
        
        GroupBox {
            title: qsTr("Display")
            CheckBox { text: qsTr("Descriptive statistics")     ; name: "descriptives" }
            CheckBox { text: qsTr("Estimates of effect size")   ; name: "effectSizeEstimates"   ; id: effectSizeEstimates }
            Row {
                Layout.leftMargin: 15
                enabled: effectSizeEstimates.checked
                CheckBox { text: qsTr("η²")         ; name: "effectSizeEtaSquared" }
                CheckBox { text: qsTr("partial η²") ; name: "effectSizePartialEtaSquared" }
                CheckBox { text: qsTr("ω²")         ; name: "effectSizeOmegaSquared" }
            }
            CheckBox { text: qsTr("Vovk-Sellke maximum p-ratio"); name: "VovkSellkeMPR" }            
        }
    }

    ExpanderButton {
        text: qsTr("Simple Main Effects")
        
        VariablesForm {
            formHeight: 200
            availableVariablesList {        title: qsTr("Factors")              ; name: "effectsVariables"      ; syncModels: "fixedFactors" }
            defaultAssignedVariablesList {  title: qsTr("Simple effect factor") ; name: "simpleFactor"          ; singleItem: true }
            AssignedVariablesList {         title: qsTr("Moderator factor 1")   ; name: "moderatorFactorOne"    ; singleItem: true }
            AssignedVariablesList {         title: qsTr("Moderator factor 2")   ; name: "moderatorFactorTwo"    ; singleItem: true }
        }
    }

    ExpanderButton {
        text: qsTr("Nonparametrics")
        
        VariablesForm {
            formHeight: 200
            availableVariablesList {        title: qsTr("Kruskal-Wallis test")  ; name: "kruskalVariablesAvailable";  syncModels: "fixedFactors" }
            defaultAssignedVariablesList {  name: "kruskalVariablesAssigned" }
        }
        
        CheckBox { text: qsTr("Dunn's post hoc test")        ; name: "dunnTest"}
    }

}
