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
            title: qsTr("Rows")
            name: "rows"
            allowedColumns: ["ordinal", "nominal"]
        }

        AssignedVariablesList {
            title: qsTr("Columns")
            name: "columns"
            allowedColumns: ["ordinal", "nominal"]
        }

        AssignedVariablesList {
            title: qsTr("Counts")
            name: "counts"
            singleItem: true
            allowedColumns: ["scale"]
        }

        AssignedVariablesList {
            title: qsTr("Layers")
            name: "layers"
            listViewType: "Layers"
            height: 120
            allowedColumns: ["ordinal", "nominal"]
        }
    }

    ExpanderButton {
        text: qsTr("Statistics")

        GridLayout {
            GroupBox {
                CheckBox { text: qsTr("χ²"); name: "chiSquared"; checked: true }
                CheckBox { text: qsTr("χ² continuity correction"); name: "chiSquaredContinuityCorrection" }
                CheckBox { text: qsTr("Likelihood ratio"); name: "likelihoodRatio" }
            }

            GroupBox {
                CheckBox { text: qsTr("Log odds ratio (2x2 only)"); name: "oddsRatio"; id: oddsRatio }
                PercentField { text: qsTr("Confidence interval"); name: "oddsRatioConfidenceIntervalInterval"; enabled: oddsRatio.checked; defaultValue: 95; indent: true }
                CheckBox { text: qsTr("Vovk-Sellke maximum p-ratio"); name: "VovkSellkeMPR" }
            }

            GroupBox {
                title: qsTr("Nominal")
                CheckBox { text: qsTr("Contingency coefficient") ; name: "contingencyCoefficient" }
                CheckBox { text: qsTr("Phi and Cramer's V")      ; name: "phiAndCramersV" }
                CheckBox { text: qsTr("Lambda")                  ; name: "lambda" ; debug: true }
                CheckBox { text: qsTr("Uncertainty coefficient") ; name: "uncertaintyCoefficient" ; debug: true }
            }

            GroupBox {
                title: qsTr("Ordinal")
                CheckBox { text: qsTr("Gamma")           ; name: "gamma" }
                CheckBox { text: qsTr("Somers' d")       ; name: "somersD" ; debug: true }
                CheckBox { text: qsTr("Kendall's tau-b") ; name: "kendallsTauB" }
                CheckBox { text: qsTr("Kendall's tau-c") ; name: "kendallsTauC" ; debug: true }
            }
        }

        GroupBox {
            debug: true
            title: qsTr("Nominal by interval")
            CheckBox { text: qsTr("Eta") ; name: "byIntervalEta"}
        }

        GroupBox {
            debug: true
            CheckBox { text: qsTr("Cochran's and Mantel-Haenszel statistics") ; name: "cochransAndMantel"; id: cochransAndMantel }
            IntegerField {
                text: qsTr("Test common odds ratio equals")
                name: "testOddsRatioEquals"
                defaultValue: 1
                enabled: cochransAndMantel.checked
                indent: true
            }
        }
    }

    ExpanderButton {
        text: qsTr("Cells")

        GridLayout {
            GroupBox {
                title: qsTr("Counts")
                CheckBox { text: qsTr("Expected"); name: "countsExpected" }
                CheckBox { text: qsTr("Hide small counts"); name: "hideSmallCounts"; id: hideSmallCounts; debug: true }
                IntegerField {
                    text: qsTr("Less than")
                    name: "hideSmallCountsLessThan"
                    defaultValue: 5
                    enabled: hideSmallCounts.checked
                    indent: true
                    debug: true
                }
            }

            GroupBox {
                title: qsTr("Z-Test")
                debug: true
                CheckBox { text: qsTr("Compare column proportions") ; name: "zTestCompareColumns"; id: zTestCompareColumns }
                CheckBox { text: qsTr("Adjust p-values"); name: "zTestAdjustPValues"; enabled: zTestCompareColumns.checked; indent: true }
            }

            GroupBox {
                title: qsTr("Percentages")
                CheckBox { text: qsTr("Row") ; name: "percentagesRow" }
                CheckBox { text: qsTr("Column") ; name: "percentagesColumn" }
                CheckBox { text: qsTr("Total") ; name: "percentagesTotal" }
            }

            GroupBox {
                title: qsTr("Residuals")
                debug: true
                CheckBox { text: qsTr("Unstandardized") ; name: "residualsUnstandardized" }
                CheckBox { text: qsTr("Standardized") ; name: "residualsStandardized" }
                CheckBox { text: qsTr("Adjusted Standardized") ; name: "residualsAdjustedStandardized" }
            }
        }
    }

    ExpanderButton {
        text: qsTr("Options")

        GridLayout {
            ButtonGroup {
                title: qsTr("Row Order")
                name: "rowOrder"
                RadioButton { text: qsTr("Ascending"); name: "ascending"; checked: true }
                RadioButton { text: qsTr("Descending"); name: "descending" }
            }
            ButtonGroup {
                title: qsTr("Column Order")
                name: "columnOrder"
                RadioButton { text: qsTr("Ascending"); name: "ascending"; checked: true }
                RadioButton { text: qsTr("Descending"); name: "descending" }
            }
        }
    }
}
