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
import JASP.Theme 1.0

Form {
	id: form
  
  	Group
	{
				
		
		RadioButtonGroup
		{
		name: "dataType"
		title: qsTr("Enter data")
			RadioButton { value: "dataCounts"; 		label: qsTr("Counts"); 			id: dataTypeA;	checked: true 	}
			RadioButton { value: "dataSequence"; 	label: qsTr("Sequence"); 		id: dataTypeB					}
			RadioButton { value: "dataVariable"; 	label: qsTr("Select variable");	id: dataTypeC					}
		}

		Group
		{
		title: qsTr("Enter count data")
		visible: dataTypeA.checked
        IntegerField { name: "nSuccesses";	label: qsTr("Number of successes");	defaultValue: 1  }
        IntegerField { name: "nFailures";	label: qsTr("Number of failures");	defaultValue: 1  }
		}
	
		Group
		{	
		title: qsTr("Enter the sequence of observation")
		visible: dataTypeB.checked
		TextField { 
			name: "data_sequence"; 
			label: "";
			placeholderText: qsTr("Enter the sequence of successes or failures (ie. '101101')")
			fieldWidth: 400 
			}
		}
		
		Group
		{	
		title: qsTr("Select variable containing the experiment")
		visible: dataTypeC.checked
		VariablesForm {
			height: 100
			AvailableVariablesList { name: "allVariables" }
			// allowedColumns: ["nominal"];
			AssignedVariablesList  { name: "selectedVariable"; label: qsTr("Selected variable"); singleVariable: true }
			}
		}
		
		Group
		{
		visible: dataTypeB.checked || dataTypeC.checked
			Group
			{
			CheckBox { name: "dataSummary"; label: qsTr("Show data summary"); checked: false }
			}
		
			RadioButtonGroup
			{
			name: "procedureType"
			title: qsTr("Select a computational procedure")
				RadioButton { value: "procedureAll"; 		label: qsTr("All in one"); 	id: procedureA;	checked: true 	}
				RadioButton { value: "procedureIterative"; 	label: qsTr("Iterative"); 	id: procedureB					}
			}
		}
	
		
	}
  
	Section
	{
		expanded: true
		title: "Prior"
		
		// table with prior
		//
		//
		
	}
	
	Section
	{
		
		expanded: true
		title: "Plots"
		Layout.columnSpan: 2
		Group{
		
			RadioButtonGroup
			{
			name: "plotsPlottingType"
			title: qsTr("Plotting type")
				RadioButton { value: "plotsFinal"; 		label: qsTr("Final"); 											id: plotsTypeA;	checked: true 	}
				RadioButton { value: "plotsOverTime"; 	label: qsTr("Over time (does not allow stacked densities)"); 	id: plotsTypeB					}
			}
			
			
			Group
			{
				CheckBox { 	name: "plotsPrior"; 	label: qsTr("Prior");		checked: false	}
				CheckBox { 	name: "plotsPosterior"; label: qsTr("Posterior");	checked: false	}
				CheckBox { 	name: "plotsBoth"; 		label: qsTr("Both");		checked: false	}
						
			RadioButtonGroup{
				name: "plotsCombinedType"
				title: qsTr("Type")
					RadioButton { value: "plotsCombinedOverlying"; 	label: qsTr("Overlying densities")	}
					Group
					{
					RadioButton { value: "plotsCombinedStacked"; 	label: qsTr("Stacked densities")	}
					}
				}		
			}
			
		}
		
	}
	
	Section
	{
		expanded: true
		title: "Prediction"
		
		Group
		{
			title: qsTr("Show prediction for")
				CheckBox { 	name: "predictionParameter"; 	label: qsTr("Paramater");		checked: false	}
				CheckBox { 	name: "predictionStatistic"; 	label: qsTr("Statistic");		checked: false	}
				CheckBox { 	name: "predictionData"; 		label: qsTr("Data");			checked: false	}
				CheckBox { 	name: "predictionComparison";	label: qsTr("Comparison");		checked: false	}
				CheckBox { 	name: "predictionAll";	 		label: qsTr("All");				checked: false	}
		}
	}
    

}
