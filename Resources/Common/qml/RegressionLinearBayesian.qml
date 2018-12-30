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

// TODO: 1. Add validators to text fields
//       2. Fix Spacing (dynamic) issues.

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
            name: "wlsWeights"
            title: qsTr("WLS Weights (optional)")
            singleItem: true
            allowedColumns: ["scale"]
        }
    }

    GridLayout {
        ColumnLayout {
            spacing: 15

            BayesFactorType { }

            ButtonGroup {
                title: qsTr("Order")
                name : "bayesFactorOrder"

                RadioButton { text: qsTr("Compare to null model"); name: "nullModelTop"; checked: true}
                RadioButton { text: qsTr("Compare to best model"); name: "bestModelTop"}
            }

            GroupBox {
                title: qsTr("Data")

                CheckBox { text: qsTr("Descriptives"); name: "descriptives" }
            }
        }

        ColumnLayout {
            spacing: 15

            GroupBox {
                title: qsTr("Output")

                CheckBox { text: qsTr("Posterior summary")   ; name: "postSummaryTable"; id: postSummaryTable }
                CheckBox { text: qsTr("Plot of coefficients"); name: "postSummaryPlot" ; id: postSummaryPlot }
                CheckBox { text: qsTr("Omit intercept")      ; name: "omitIntercept"   ; Layout.leftMargin: 20; checked: true; enabled: postSummaryPlot.checked }

                RowLayout {
                    enabled: postSummaryTable.checked || postSummaryPlot.checked

                    Label { text: qsTr("Summary type ") }
                    ComboBox {
                        name   : "summaryType"
                        model  : [ "best", "complex", "median", "averaged" ]
                    }
                }

                PercentField {
                    label.text  : qsTr("Credible interval")
                    name        : "descriptivesPlotsCredibleInterval"
                    defaultValue: 95
                    enabled     : postSummaryTable.checked || postSummaryPlot.checked
                }

            }

            ButtonGroup {
                title: qsTr("Limit no. models shown")
                name: "shownModels"

                RadioButton { text: qsTr("No"); name: "unlimited" }
                RowLayout {
                    RadioButton { text: qsTr("Yes, show best"); name: "limited"; id: limited; checked: true }
                    TextField   { text: "10"; name: "numShownModels"; inputType: "integer"; enabled: limited.checked }
                }
            }
        }
    }

    ExpanderButton {
        text: qsTr("Model")

        VariablesForm {
            height: 200
            availableVariablesList {
                title: qsTr("Components")
                name: "components"
                syncModels: ["covariates"]
            }

            AssignedVariablesList {
                title: qsTr("Model terms")
                name: "modelTerms"
                listViewType: "AssignedAnova"

                ExtraControlColumn {
                    type : "CheckBox"
                    name : "isNuisance"
                    title: "Add to null model"
                }
            }
        }
    }

    ExpanderButton {
        text: qsTr("Plots")

        GridLayout {
            ColumnLayout {
                spacing: 15

                GroupBox {
                    title: qsTr("Coefficients")

                    CheckBox { text: qsTr("Inclusion probabilities")         ; name: "plotInclusionProbabilities" }
                    CheckBox { text: qsTr("Marginal posterior distributions"); name: "plotCoefficientsPosterior" }
                }

                GroupBox {
                    title: qsTr("Residuals")

                    CheckBox { text: qsTr("Residuals vs. Fitted"); name: "plotResidualsVsFitted" }
                }
            }

            ColumnLayout {
                spacing: 15

                GroupBox {
                    title: qsTr("Models")

                    CheckBox { text: qsTr("Log posterior odds"); name: "plotLogPosteriorOdds" }
                    CheckBox { text: qsTr("Log(P(data | M)) vs. model size"); name: "plotModelComplexity" }
                    CheckBox { text: qsTr("Model probabilities"); name: "plotModelProbabilities" }
                }
            }
        }
    }

    ExpanderButton {
        text: qsTr("Advanced Options")

        GridLayout {
            ColumnLayout {
                spacing: 15

                ButtonGroup {
                    title: qsTr("Prior")
                    name : "priorRegressionCoefficients"

                    RadioButton { text: qsTr("AIC")      ; name: "AIC" }
                    RadioButton { text: qsTr("BIC")      ; name: "BIC" }
                    RadioButton { text: qsTr("EB-global"); name: "EB-global" }
                    RadioButton { text: qsTr("EB-local") ; name: "EB-local" }
                    RadioButton { text: qsTr("g-prior")  ; name: "g-prior" }
                    RadioButton { text: qsTr("Hyper-g")  ; name: "hyper-g"; id: hyperG }
                    RowLayout {
                        RadioButton { text: qsTr("Hypger-g-Laplace"); name: "hyper-g-laplace"; id: hyperGLaplace }

                        Label {
                            text: qsTr("alpha")
                            visible: hyperGLaplace.checked || hyperG.checked || hyperGN.checked
                        }
                        TextField {
                            text: "0.354"
                            name: "alpha"
                            inputType: "number"
                            visible: hyperGLaplace.checked || hyperG.checked || hyperGN.checked
                        }
                    }
                    RadioButton { text: qsTr("Hyper-g-n"); name: "hyper-g-n"; id: hyperGN }
                    RowLayout {
                        RadioButton { text: qsTr("JZS"); name: "JZS"; id: jzs; checked: true }

                        Label { text: qsTr("r scale"); visible: jzs.checked } TextField { text: "0.354" ; name: "rScale"; inputType: "number"; visible: jzs.checked }
                    }
                }
            }

            ColumnLayout {
                spacing: 15

                ButtonGroup {
                    title: qsTr("Model prior")
                    name : "modelPrior"

                    RowLayout {
                        RadioButton { text: qsTr("Beta binomial"); name: "beta.binomial"; id: betaBinomial; checked: true }

                        Label { text: qsTr("a"); visible: betaBinomial.checked } TextField { text: "1" ; name: "betaBinomialParamA"; inputType: "number"; visible: betaBinomial.checked }
                        Label { text: qsTr("b"); visible: betaBinomial.checked } TextField { text: "1" ; name: "betaBinomialParamB"; inputType: "number"; visible: betaBinomial.checked }
                    }
                    RowLayout {
                        RadioButton { text: qsTr("Bernoulli"); name: "Bernoulli"; id: bernoulli }

                        Label { text: qsTr("p"); visible: bernoulli.checked } TextField { text: "0.5" ; name: "bernoulliParam"; inputType: "number"; visible: bernoulli.checked }
                    }
                    RadioButton { text: qsTr("Uniform"); name: "uniform" }
                }

                ButtonGroup {
                    title: qsTr("Sampling method")
                    name : "samplingMethod"

                    RowLayout {
                        RadioButton { text: qsTr("BAS"); name: "BAS"; id: bas; checked: true }

                        Label { text: qsTr("No. models"); visible: bas.checked } TextField { text: "0" ; name: "numberOfModels"; inputType: "integer"; visible: bas.checked }
                    }
                    RowLayout {
                        RadioButton { text: qsTr("MCMC"); name: "MCMC"; id: mcmc }

                        Label { text: qsTr("No. samples"); visible: mcmc.checked } TextField { text: "0" ; name: "iterationsMCMC"; inputType: "integer"; visible: mcmc.checked }
                    }
                }

                GroupBox {
                    title: qsTr("Numerical accuracy")

                    Label { text: qsTr("No. samples for credible interval") } TextField { text: "1000" ; name: "nSimForCRI"; inputType: "integer" }
                }
            }
        }
    }
}
