import QtQuick 2.8
import QtQuick.Layouts 1.3
import JASP.Controls 1.0

Form {
    id: form

    VariablesForm {
        formHeight: 200
        defaultAssignedVariablesList {
            title: qsTr("Dependent Variables")
            allowedColumns: ["scale"]
        }
        AssignedVariablesList {
            name: "groupingVariable"
            title: qsTr("Grouping Variable")
            singleItem: true
            allowedColumns: ["ordinal", "nominal"]
        }
    }
    
    GridLayout {
        ColumnLayout {
            spacing: 15
            ButtonGroup {
                title: qsTr("Hypothesis")                       ; name: "hypothesis"
                RadioButton {   text: qsTr("Group 1 ≠ Group 2") ; name: "groupsNotEqual"       ; checked: true}
                RadioButton {   text: qsTr("Group 1 > Group 2") ; name: "groupOneGreater"    }
                RadioButton {   text: qsTr("Group 1 < Group 2") ; name: "groupTwoGreater"    }
            }
            
            ButtonGroup {
                title: qsTr("Bayes Factor")                     ; name: "bayesFactorType"
                RadioButton {  text: qsTr("BF\u2081\u2080")       ; name: "BF10"          ; checked: true}
                RadioButton {  text: qsTr("BF\u2080\u2081")       ; name: "BF01"  }
                RadioButton {  text: qsTr("Log(BF\u2081\u2080)")  ; name: "LogBF10"  }
            }
            
            ButtonGroup {
                title: qsTr("Tests")                            ; name: "testStatistic"
                RadioButton {   text: qsTr("Student")           ; name: "Student";  checked: true}
                RadioButton {   text: qsTr("Mann-Whitney")      ; name: "Wilcoxon"; id: wilcoxon}
                Row {
                    spacing: 5
                    enabled: wilcoxon.checked
                    Layout.leftMargin: 25
                    Label { text: qsTr("No. samples")}
                    TextField { text: "1000"; name: "wilcoxonSamplesNumber";inputType: "integer"; validator: IntValidator {bottom: 100; top: 10000}}
                }
            }
            
            GroupBox {
                title: qsTr("Assumption checks")
                CheckBox {  text: qsTr("Descriptives")                 ; name: "descriptives"   }
            }
        }            
            
        ColumnLayout {
            spacing: 15
            GroupBox {
                title: qsTr("Plots")
                CheckBox {  text: qsTr("Prior and posterior") ; name: "plotPriorAndPosterior"; id: plotPriorAndPosterior }
                CheckBox {  text: qsTr("Additional info")     ; name: "plotPriorAndPosteriorAdditionalInfo"; Layout.leftMargin: 20; checked: true; enabled: plotPriorAndPosterior.checked}

                CheckBox {  text: qsTr("Bayes factor robustness check") ; name: "plotBayesFactorRobustness"; id: plotBayesFactorRobustness }
                CheckBox {  text: qsTr("Additional info")     ; name: "plotBayesFactorRobustnessAdditionalInfo"; Layout.leftMargin: 20; checked: true; enabled: plotBayesFactorRobustness.checked}

                CheckBox {  text: qsTr("Sequential analysis") ; name: "plotSequentialAnalysis"; id: plotSequentialAnalysis }
                CheckBox {  text: qsTr("Robustness check")    ; name: "plotSequentialAnalysisRobustness"; Layout.leftMargin: 20; enabled: plotSequentialAnalysis.checked}

                CheckBox {  text: qsTr("Descriptives plots")  ; name: "descriptivesPlots"; id: descriptivesPlots }
                PercentField { label.text: qsTr("Credible interval") ; name: "descriptivesPlotsCredibleInterval"; defaultValue: 95; Layout.leftMargin: 20; enabled: descriptivesPlots.checked}
            }
            
            ButtonGroup {
                title: qsTr("Missing Values"); name: "missingValues"
                RadioButton {   text: qsTr("Exclude cases analysis by analysis")    ; name: "excludeAnalysisByAnalysis" ; checked: true }
                RadioButton {   text: qsTr("Exclude cases listwise")                ; name: "excludeListwise"    }
            }         
        }
    }

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
    
}
