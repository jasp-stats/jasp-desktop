//
// Copyright (C) 2013-2019 University of Amsterdam
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

import QtQuick			2.8
import QtQuick.Layouts	1.3
import JASP.Controls	1.0
import JASP.Widgets		1.0
import JASP.Theme		1.0
import "./common" as ML

Form {

    VariablesForm {
        AvailableVariablesList { name: "allVariablesList" }
        AssignedVariablesList  { 
            name: "target"    
            title: qsTr("Target")         
            singleVariable: true
            allowedColumns: ["nominal", "nominalText", "ordinal"]
            id: target 
        }
        AssignedVariablesList  { 
            name: "predictors"
            title: qsTr("Predictors")
            allowedColumns: ["scale", "ordinal", "nominal"]
            id: predictors                                        
        }
    }

    ColumnLayout {

        GroupBox {
            title: qsTr("Tables")

            CheckBox { 
                text: qsTr("Confusion matrix") 
                name: "confusionTable"
                checked: true

                CheckBox { 
                    text: qsTr("Display proportions")
                    name: "confusionProportions"
                } 
            }

            CheckBox {
                text: qsTr("Class proportions")
                name: "classProportionsTable"
            }  

            CheckBox {
                text: qsTr("Evaluation metrics")
                name: "validationMeasures"
            }  

            CheckBox { 
                name: "coefficientsTable"
                text: qsTr("Coefficients")       
            }

            CheckBox { 
                name: "priorTable"
                text: qsTr("Prior and posterior probabilities")
            }

            CheckBox { 
                name: "meanTable"
                text: qsTr("Class means training data")             
            }
        }

        Divider { }

        GroupBox {
            title: qsTr("Assumption Checks")

            CheckBox { 
                name: "manovaTable"
                text: qsTr("Equality of class means")             
            }

            CheckBox { 
                name: "boxTest"
                text: qsTr("Equality of covariance matrices")     
            }

            CheckBox { 
                name: "multicolTable"
                text: qsTr("Multicollinearity")                   
            }
        }
    }

    GroupBox {
        title: qsTr("Plots")

        CheckBox { 
            text: qsTr("Data split") 
            name: "dataSplitPlot"
            checked: true
        }

        CheckBox { 
            name: "rocCurve"
            text: qsTr("ROC curves") 
        }

        CheckBox { 
            name: "andrewsCurve"
            text: qsTr("Andrews curves") 
        }

        CheckBox { 
            name: "matrixplot"
            text: qsTr("Linear discriminant matrix")

            RowLayout {

                CheckBox { 
                    name: "plotDensities"
                    text:qsTr("Densities")
                    checked: true
                }

                CheckBox { 
                    name: "plotStatistics"
                    text: qsTr("Scatter plots")
                    checked: true
                }
            }
        }

        CheckBox { 
            name: "decisionBoundary"
            text: qsTr("Decision boundary matrix")

            RowLayout {

                CheckBox {
                    name: "plotLegend"
                    text: qsTr("Legend")
                    checked: true 
                }

                CheckBox {
                    name: "plotPoints"
                    text: qsTr("Points")
                    checked: true 
                }
            }
        }
    }

    ML.DataSplit {
        leaveOneOutVisible: false; 
        kFoldsVisible: false
    }

    Section {
        title: qsTr("Training Parameters")

        GroupBox {
            title: qsTr("Algorithmic Settings")

            DropDown {
                name: "estimationMethod"
                indexDefaultValue: 0
                label: qsTr("Estimation method:")
                values:
                [
                    { label: "Moment", value: "moment"},
                    { label: "MLE", value: "mle"},
                    { label: "MVE", value: "covMve"},
                    { label: "t", value: "t"},
                ]
            }

            CheckBox { 
                text: qsTr("Scale predictors") 
                name: "scaleEqualSD"
                checked: true
            }

            CheckBox { 
                name: "seedBox"
                text: qsTr("Set seed: ")
                childrenOnSameRow: true

                DoubleField  { 
                    name: "seed"
                    defaultValue: 1
                    min: -999999
                    max: 999999
                    fieldWidth: 60 
                } 
            }
        }
    }

    Item {
        height: 			saveModel.height
        Layout.fillWidth: 	true
        Layout.columnSpan: 2

        Button 
        {
            id: 			saveModel
            anchors.right: 	parent.right
            text: 			qsTr("<b>Save Model</b>")
            enabled: 		predictors.count > 0 && target.count > 0
            onClicked:      
            {
                
             }
            debug: true	
        }
    }
}

