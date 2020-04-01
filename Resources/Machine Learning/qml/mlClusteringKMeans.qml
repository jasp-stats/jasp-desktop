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


Form {

    VariablesForm {
        AvailableVariablesList { name: "variables"}
        AssignedVariablesList {
            id: predictors
            name: "predictors"
            title: qsTr("Variables")
            allowedColumns: ["scale"]
			allowAnalysisOwnComputedColumns: false
        }
    }

    GroupBox {
        title: qsTr("Tables")

        CheckBox { 
            text: qsTr("Cluster means") 
            name: "tableClusterMeans" 
        }

        CheckBox { 
            id: clusterInfo
            text: qsTr("Cluster information") 
            name: "tableClusterInformation" 
            checked: true

            CheckBox { 
                text: qsTr("Within sum of squares")
                name: "tableClusterInfoWSS" 
                checked: true
            }

            CheckBox { 
                text: qsTr("Silhouette score")
                name: "tableClusterInfoSilhouette"
            }

            CheckBox { 
                text: qsTr("Centroids")
                name: "tableClusterInfoCentroids" 
            }

            CheckBox { 
                text: qsTr("Between sum of squares") 
                name: "tableClusterInfoBetweenSumSquares" 
            }

            CheckBox { 
                text: qsTr("Total sum of squares")
                name: "tableClusterInfoTotalSumSquares" 
            }
        }

        CheckBox {
            text: qsTr("Evaluation metrics")
            name: "clusterEvaluationMetrics"
        }
    }

    GroupBox {
        title: qsTr("Plots")

        CheckBox { 
            text: qsTr("Elbow method")  
            name: "withinssPlot" 
            enabled: !validationManual.checked
        }

        CheckBox { 
            text: qsTr("Cluster means") 
            name: "plotClusterMeans" 

            CheckBox {
                text: qsTr("Display barplot")
                name: "showBars"
                checked: true
            }

            CheckBox {
                text: qsTr("Group into one figure")
                name: "oneFigure"
                checked: true
            }
        }

        CheckBox { 
            text: qsTr("Cluster densities") 
            name: "plotClusterDensities" 
        }

        CheckBox { 
            text: qsTr("t-SNE cluster plot")     
            name: "plot2dCluster" 

            RowLayout {

                CheckBox {
                    text: qsTr("Legend")  
                    name: "legend"
                    checked: true 
                } 
                
                CheckBox {
                    text: qsTr("Labels")  
                    name: "labels"
                }
            }
        }
    }

    Section {
        title: qsTr("Training Parameters")

        GroupBox {
            title: qsTr("Algorithmic Settings")

            IntegerField { 
                name: "noOfIterations"
                text: qsTr("Max. iterations:") 
                defaultValue: 25 
                min: 1
                max: 999999
                fieldWidth: 60 
            }

            IntegerField { 
                name: "noOfRandomSets"
                text: qsTr("Random sets:") 
                defaultValue: 25 
                min: 1
                max: 999999
                fieldWidth: 60 
            }

            DropDown {
                name: "algorithm"
                indexDefaultValue: 0
                label: qsTr("Algorithm:")
                values:
                [
                    { label: "Hartigan-Wong", value: "Hartigan-Wong"},
                    { label: "Lloyd-Forgy", value: "Lloyd"},
                    { label: "MacQueen", value: "MacQueen"}
                ]
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
            title: qsTr("Cluster Determination")
            name: "modelOpt"

            RadioButton { 
                id: validationManual 
                text: qsTr("Fixed")                          
                name: "validationManual"

                IntegerField { 
                    name: "noOfClusters"
                    text: qsTr("Clusters:") 
                    defaultValue: 3 
                    min: 2
                    max: 5000
                    fieldWidth: 60
                    enabled: validationManual.checked 
                }
            }

            RadioButton { 
                text: qsTr("Optimized according to")                             
                name: "validationOptimized" 
                childrenOnSameRow: true
                checked: true

                DropDown {
                    name: "optimizationCriterion"
                    indexDefaultValue: 1
                    
                    values:
                    [
                        { label: "AIC", value: "validationAIC"},
                        { label: "BIC", value: "validationBIC"},
                        { label: "Silhouette", value: "validationSilh"}
                    ]
                } 
            }

            IntegerField { 
                    name: "maxClusters"
                    text: qsTr("Max. clusters:") 
                    defaultValue: 10 
                    min: 2
                    max: 5000
                    fieldWidth: 60
                    enabled: !validationManual.checked 
                    Layout.leftMargin: 20
                }
        }
    }

    Item {
        Layout.preferredHeight: 	addClusters.height*2
        Layout.fillWidth: 	true
        Layout.columnSpan: 2

        CheckBox {
            id: addClusters
            name: "addClusters"
            text: qsTr("Add predicted clusters to data")
            enabled:    predictors.count > 1
            anchors.top: parent.top

            ComputedColumnField { 
                id: 		clusterColumn
                name: 		"clusterColumn"
				text: 		qsTr("Column name: ")
                fieldWidth: 120
                visible:    addClusters.checked
            }

        }

        Button 
        {
            id: 			saveModel
            anchors.right: 	parent.right
            text: 			qsTr("<b>Save Model</b>")
            enabled: 		predictors.count > 1
            onClicked:      
            {
                
             }
            debug: true	
        }
    }
}
