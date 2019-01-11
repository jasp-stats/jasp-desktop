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
import JASP.Theme 1.0

Form {
    id: form
    
    VariablesForm {
        defaultAssignedVariablesList {
            name: "dependent"
            title: qsTr("Dependent Variable")
            singleItem: true
            allowedColumns: ["scale"]
        }

        AssignedVariablesList {
            name: "covariates"
            title: qsTr("Covariates")
            allowedColumns: ["scale"]
        }
        AssignedVariablesList {
            name: "factors"
            debug: true
            title: qsTr("Factors")
            allowedColumns: ["nominal", "ordinal"]
        }
        AssignedVariablesList {
            name: "wlsWeights"
            title: qsTr("WLS Weights (optional)")
            singleItem: true
            allowedColumns: ["scale"]
        }
    }

    ComboBox {
        text: qsTr("Method")
        name: "method"
        model: ListModel {
            ListElement { key: "Enter"; value: "enter" }
            ListElement { key: "Backward"; value: "backward" }
            ListElement { key: "Forward"; value: "forward" }
            ListElement { key: "Stepwise"; value: "stepwise" }
        }
    }

    ExpanderButton {
        text: qsTr("Model")

        VariablesForm {
            height: 200
            showDefaultAssignedVariablesList: false // Cannot use defaultAssignedVariablesList with an ExtraControlColumn
            listWidth: parent.width * 5 / 9

            availableVariablesList {
                name: "availableTerms"
                title: qsTr("Components")
                width: parent.width / 4
                syncModels: ['covariates', 'factors']
            }
            AssignedVariablesList {
                name: "modelTerms"
                title: qsTr("Model terms")
                listViewType: "Interaction"
                ExtraControlColumn {
                    type: "CheckBox"
                    name: "isNuisance"
                    title: qsTr("Add to null model")
                }
            }
        }

    }

    ExpanderButton {
        text: qsTr("Statistics")

        GroupBox {
            title: qsTr("Regression Coefficients")

            GridLayout {
                GroupBox {
                    CheckBox { text: qsTr("Estimates"); name: "regressionCoefficientsEstimates"; checked: true; id: regressionCoefficientsEstimates }
                    RowLayout {
                        enabled: regressionCoefficientsEstimates.checked
                        Layout.leftMargin: Theme.indentationLength
                        CheckBox { text: qsTr("From"); name: "regressionCoefficientsBootstrapping" }
                        IntegerField {
                            name: "regressionCoefficientsBootstrappingReplicates"
                            defaultValue: 5000
                            fieldWidth: 50
                            intValidator.bottom: 100
                            afterLabel.text: qsTr("bootstraps")
                        }
                    }

                    RowLayout {
                        CheckBox { text: qsTr("Confidence intervals"); name: "regressionCoefficientsConfidenceIntervals" }
                        PercentField { name: "regressionCoefficientsConfidenceIntervalsInterval"; defaultValue: 95 }
                    }
                    CheckBox { text: qsTr("Covariance matrix"); name: "regressionCoefficientsCovarianceMatrix" }
                }

                GroupBox {
                    CheckBox { text: qsTr("Model fit"); name: "modelFit"; checked: true }
                    CheckBox { text: qsTr("R squared change"); name: "rSquaredChange" }
                    CheckBox { text: qsTr("Descriptives"); name: "descriptives" }
                    CheckBox { text: qsTr("Part and partial correlations"); name: "partAndPartialCorrelations" }
                    CheckBox { text: qsTr("Collinearity diagnostics"); name: "collinearityDiagnostics" }
                }
            }
        }

        GroupBox {
            title: qsTr("Residuals")
            CheckBox { text: qsTr("Dublin-Watson"); name: "residualsDurbinWatson" }
            CheckBox { text: qsTr("Casewise diagnostics"); name: "residualsCasewiseDiagnostics"; id: residualsCasewiseDiagnostics }
            RadioButtonGroup {
                name: "residualsCasewiseDiagnosticsType"
                enabled: residualsCasewiseDiagnostics.checked
                indent: true
                RowLayout {
                    RadioButton { text: qsTr("Standard residual >"); name: "outliersOutside"; checked: true }
                    IntegerField { name: "residualsCasewiseDiagnosticsOutliersOutside"; defaultValue: 3 }
                }
                RowLayout {
                    RadioButton { text: qsTr("Cook's distance >"); name: "cooksDistance" }
                    IntegerField { name: "residualsCasewiseDiagnosticsCooksDistance"; defaultValue: 0 }
                }
                RadioButton { text: qsTr("All"); name: "allCases" }
            }
        }

    }
    
    ExpanderButton {
        text: qsTr("Options")

        CheckBox { text: qsTr("Vovk-Sellke maximum p-ratio"); name: "VovkSellkeMPR" }

        RadioButtonGroup {
            title: qsTr("Stepping Method Criteria")
            name: "steppingMethodCriteriaType"
            RadioButton { text: qsTr("Use p value"); name: "usePValue"; checked: true; id: usePValue}
            RowLayout {
                enabled: usePValue.checked
                Layout.leftMargin: Theme.indentationLength
                DoubleField { text: qsTr("Entry"); name: "steppingMethodCriteriaPEntry"; defaultValue: 0.05; doubleValidator { top: 1; decimals: 3} }
                DoubleField { text: qsTr("Removal"); name: "steppingMethodCriteriaPRemoval"; defaultValue: 0.1; doubleValidator { top: 1; decimals: 3} }
            }
            RadioButton { text: qsTr("Use F value"); name: "useFValue"; id: useFValue}
            RowLayout {
                enabled: useFValue.checked
                Layout.leftMargin: Theme.indentationLength
                DoubleField { text: qsTr("Entry"); name: "steppingMethodCriteriaFEntry"; defaultValue: 3.84; doubleValidator.decimals: 3 }
                DoubleField { text: qsTr("Removal"); name: "steppingMethodCriteriaFRemoval"; defaultValue: 2.71; doubleValidator.decimals: 3 }
            }
        }

        CheckBox { text: qsTr("Include constant in equation"); name: "includeConstant"; checked: true }

        RadioButtonGroup {
            title: qsTr("Missing Values")
            debug: true
            name: "missingValues"
            RadioButton { text: qsTr("Exclude cases listwise"); name: "excludeCasesListwise"; checked: true }
            RadioButton { text: qsTr("Exclude cases pairwise"); name: "excludeCasesPairwise" }
        }
    }

    ExpanderButton {
        text: qsTr("Plots")

        GroupBox {
            title: qsTr("Residuals Plots")
            CheckBox { text: qsTr("Residuals vs. dependent"); name: "plotResidualsDependent" }
            CheckBox { text: qsTr("Residuals vs. covariates"); name: "plotResidualsCovariates" }
            CheckBox { text: qsTr("Residuals vs. predicted"); name: "plotResidualsPredicted" }
            CheckBox { text: qsTr("Residuals vs. histogram"); name: "plotResidualsHistogram"; id: plotResidualsHistogram }
            CheckBox { text: qsTr("Standardized residuals"); name: "plotResidualsHistogramStandardized"; enabled: plotResidualsHistogram.checked; indent: true }
            CheckBox { text: qsTr("Q-Q plot standardized residuals"); name: "plotResidualsQQ" }
            CheckBox { text: qsTr("Partial plots"); name: "plotsPartialRegression" }
        }
    }
}
