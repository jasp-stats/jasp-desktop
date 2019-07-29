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

Form {

    VariablesForm {
        AvailableVariablesList { name: "allVariablesList" }
        AssignedVariablesList  { 
            id: target
            name: "target"     
            title: qsTr("Target")         
            singleVariable: true
            allowedColumns: ["scale"]  
        }
        AssignedVariablesList { 
            id: predictors
            name: "predictors" 
            title: qsTr("Predictors")
            allowedColumns: ["scale", "nominal", "ordinal", "nominalText"] 
        }
    }

    GroupBox {
        title: qsTr("Tables")

        CheckBox {
            text: qsTr("Evaluation metrics")
            name: "validationMeasures"
        }  

        CheckBox { 
            name: "classBoostRelInfTable"
            text: qsTr("Relative influence")            
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
            name: "plotOOBChangeDev"
            text: qsTr("Out-of-bag improvement")      
        }

        CheckBox { 
            text: qsTr("Predictive performance") 
            name: "predictedPerformancePlot"
        }

        CheckBox { 
            name: "plotDeviance"
            text: qsTr("Deviance")             
        }

        CheckBox { 
            name: "plotRelInf"
            text: qsTr("Relative influence")   
        }
    }

    Section {
        title: qsTr("Data Split Preferences")

        RadioButtonGroup {
            title: qsTr("Holdout Test Data")
            name: "holdoutData"

            RadioButton {
                id: holdoutManual
                name: "holdoutManual"
                childrenOnSameRow: true
                text: qsTr("Sample")

                RowLayout {
                
                    PercentField {    
                        name: "testDataManual"
                        defaultValue: 20
                        min: 5
                        max: 95 
                    }

                    Text {
                        text: qsTr("of all data")
                        enabled: true
                    }
                }
            }

            CheckBox { 
                id: addIndicator  
                name: "addIndicator"
                text: qsTr("Add generated indicator to data")
                Layout.leftMargin: 20
                enabled: holdoutManual.checked

                ComputedColumnField { 
                    name: 		"testIndicatorColumn"
                    text: 		"Name: "
                    fieldWidth: 120
                    visible:    addIndicator.checked
                }
            }

            RadioButton {
                id: testSetIndicator
                name: "testSetIndicator"
                label: qsTr("Test set indicator:")
                childrenOnSameRow: true

                DropDown {
                    name: "testSetIndicatorVariable"
                    showVariableTypeIcon: true
                    addEmptyValue: true
                    placeholderText: qsTr("None")
                }
            }
        }

        RadioButtonGroup {
            title: qsTr("Training and Validation Data")
            name: "modelValid"

            RadioButton {
                name: "validationManual"
                childrenOnSameRow: true
                checked: true
                text: qsTr("Sample")

                RowLayout {

                    PercentField {     
                        name: "validationDataManual"
                        defaultValue: 20
                        min: 5
                        max: 95
                    }

                    Text {
                        text: qsTr("for validation data")
                        enabled: true
                    }
                }
            }
    
            RadioButton { 
                name: "validationKFold"
                childrenOnSameRow: true
                text: qsTr("K-fold with")

                RowLayout {

                    IntegerField {
                        name: "noOfFolds"
                        defaultValue: 5
                        min: 2
                        max: 999
                        fieldWidth: 30
                    } 

                    Text {
                        text: qsTr("folds")
                        enabled: true
                    }
                }
            }
        }
    }

    Section {
        title: qsTr("Training Parameters")
  
        GroupBox {
            title: qsTr("Algorithmic Settings")

            DoubleField  { 
                name: "shrinkage"
                text: qsTr("Shrinkage:")                    
                defaultValue: 0.1 
                min: 0
                max: 1     
                fieldWidth: 60 
            }

            IntegerField { 
                name: "intDepth" 
                text: qsTr("Interaction depth:")            
                defaultValue: 1   
                min: 1
                max: 99    
                fieldWidth: 60 
            }

            IntegerField { 
                name: "nNode"    
                text: qsTr("Min. observations in node:")
                defaultValue: 10  
                min: 1
                max: 999999
                fieldWidth: 60 
            }

            PercentField { 
                name: "bagFrac"  
                text: qsTr("Training data used per tree:")  
                defaultValue: 50                                        
            }

            DropDown {
                    name: "distance"
                    indexDefaultValue: 0
                    label: qsTr("Loss function:")
                    
                    values:
                    [
                        { label: "Gaussian", value: "gaussian"},
                        { label: "Laplace", value: "laplace"},
                        { label: "t", value: "tdist"}
                    ]
            }

            CheckBox { 
                text: qsTr("Scale predictors") 
                name: "scaleEqualSD"
                checked: true
            }

            CheckBox { 
                name: "seedBox"
                text: qsTr("Set seed:")
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

        RadioButtonGroup {
            title: qsTr("Number of Trees")
            name: "modelOpt"

            RadioButton { 
                text: qsTr("Fixed")                     
                name: "optimizationManual" 

                IntegerField { 
                    name: "noOfTrees"
                    text: qsTr("Trees:")
                    defaultValue: 100
                    min: 1
                    max: 999999
                    fieldWidth: 60
                }
            }
            
            RadioButton { 
                text: qsTr("Optimized")
                name: "optimizationOOB"
                checked: true 

                IntegerField { 
                    name: "maxTrees"
                    text: qsTr("Max. trees:") 
                    defaultValue: 100
                    min: 1
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
            enabled: 		predictors.count > 1 && target.count > 0
            onClicked:      
            {

             }
            debug: true	
        }
    }
}
