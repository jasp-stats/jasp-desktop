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

import QtQuick			2.8
import QtQuick.Layouts	1.3
import JASP.Controls	1.0
import JASP.Widgets		1.0

import "./common" as ML

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
			allowedColumns: ["scale", "nominal", "nominalText", "ordinal"]
			allowAnalysisOwnComputedColumns: false
        }
    }

    GroupBox {
        title: qsTr("Tables")

        CheckBox {
            text: qsTr("Evaluation metrics")
            name: "validationMeasures"
        }  

        CheckBox { 
            name: "tableVariableImportance"
            text: qsTr("Variable importance") 
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
            name: "plotTreesVsModelError"
            text: qsTr("Out-of-bag error")         
        }

        CheckBox { 
            name: "predictedPerformancePlot"         
            text: qsTr("Predictive performance")	      
        }

        CheckBox { 
            name: "plotDecreaseAccuracy"        
            text: qsTr("Mean decrease in accuracy")     
        }

        CheckBox { 
            name: "plotIncreasePurity"        
            text: qsTr("Total increase in node purity") 
        }

    }

    ML.DataSplit {
        leaveOneOutVisible: false
        kFoldsVisible: false
        trainingValidationSplit: optimizeModel.checked 
    }

    Section {
        title: qsTr("Training Parameters")

        GroupBox {
            title: qsTr("Algorithmic Settings")

            PercentField { 
                name: "bagFrac"       
                text: qsTr("Training data used per tree:")
                defaultValue: 50 
                min: 5
                max: 95
            }
            
            RowLayout {

                DropDown {
                    id: noOfPredictors
                    name: "noOfPredictors"
                    indexDefaultValue: 0
                    label: qsTr("Predictors per split:")
                    values:
                    [
						{ label: qsTr("Auto"), value: "auto"},
						{ label: qsTr("Manual"), value: "manual"}
                    ]
                } 

                IntegerField  { 
                    name: "numberOfPredictors"
                    defaultValue: 1
                    min: 1
                    max: 5000
                    visible: noOfPredictors.currentIndex == 1 
                }
            }

            CheckBox { 
                text: qsTr("Scale variables") 
                name: "scaleEqualSD"
                checked: true
            }

            CheckBox { 
                name: "seedBox"
                text: qsTr("Set seed:")
                childrenOnSameRow: true

                DoubleField { 
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
                    max: 500000
                    fieldWidth: 60
                }
            }
            
            RadioButton { 
                id: optimizeModel
                text: qsTr("Optimized")
                name: "optimizationError"
                checked: true 

                IntegerField { 
                    name: "maxTrees"
                    text: qsTr("Max. trees:") 
                    defaultValue: 100 
                    min: 1
                    max: 500000
                    fieldWidth: 60
                }
            }
        }
    }

    Item {
		Layout.preferredHeight: addValues.height*2
        Layout.fillWidth: 	true
        Layout.columnSpan: 2

        CheckBox {
            id: addValues
            name: "addValues"
            text: qsTr("Add predicted values to data")
            enabled:    predictors.count > 1 && target.count > 0
            anchors.top: parent.top

            ComputedColumnField { 
                id: 		valueColumn
                name: 		"valueColumn"
				text: 		qsTr("Column name: ")
                fieldWidth: 120
                visible:    addValues.checked
            }

        }

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
