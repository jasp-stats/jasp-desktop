//
// Copyright (C) 2013-2020 University of Amsterdam
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

import QtQuick 			2.8
import JASP.Controls 	1.0
import JASP.Theme 		1.0
import JASP.Widgets		1.0

Form
{
  	VariablesForm
  	{
  		height: 300

  		AvailableVariablesList { name: "allVariablesList" }		
  		
		AssignedVariablesList 	
		{ 
			name: 			"variables"
			title: 			qsTr("Variables")
			allowedColumns: ["scale", "ordinal"]
		}
  	}
  	
  	Section
    {
    	title: qsTr("Single-Test Reliability")
    	
		Group
    	{
    		title: qsTr("Scale Statistics")

    		CIField 
    		{      
				name: 			"credibleIntervalValueScale";   
				label: 			qsTr("Credible interval");
				defaultValue: 	95
    		}

    		CheckBox 
    		{    
				id:     	mcdonald
				name: 		"mcDonaldScale"	
				label:  	qsTr("McDonald's ω")         
				checked: 	true

				CheckBox 
				{    
					name:   	"dispPPC"	
					label:  	qsTr("Posterior predictive check"); 
					enabled: 	mcdonald.checked        
				}
    
    		}

			CheckBox 
			{     
				id: 	cronbach   		      
				name: 	"alphaScale";				
				label: 	qsTr("Cronbach's α");         
			}
			
			CheckBox 
			{     
				id: 	guttman2      
				name: 	"guttman2Scale";			
				label: 	qsTr("Guttman's λ2");        
			}
			
			CheckBox 
			{     
				id: 	guttman6      
				name: 	"guttman6Scale";			
				label: 	qsTr("Guttman's λ6");        
			}
			
			CheckBox 
			{     
				id: 	glb      	              
				name: 	"glbScale";				  
				label: 	qsTr("Greatest lower bound"); 
			}

			CheckBox { name: "averageInterItemCor";	label: qsTr("Average interitem correlation")	}
			CheckBox { name: "meanScale";			label: qsTr("Mean")								}
			CheckBox { name: "sdScale";				label: qsTr("Standard deviation")				}
        
    	}
           
    	Group
    	{
    		title: qsTr("Individual Item Statistics")

    		CIField 
    		{      
				name: 			"credibleIntervalValueItem";   
				label: 			qsTr("Credible interval");
				defaultValue: 	95
    		}

    		CheckBox 
    		{ 
				id: 		mcdonaldItem
				name: 		"mcDonaldItem";				
				label: 		qsTr("McDonald's ω  (if item dropped)");	        
				enabled: 	mcdonald.checked 
    		}
    		
			CheckBox 
    		{ 
				id: 		cronbachItem
				name: 		"alphaItem";					
				label: 		qsTr("Cronbach's α (if item dropped)");	        
				enabled: 	cronbach.checked 
			}
    		
			CheckBox 
    		{ 
				id: 		lambda2Item
				name: 		"guttman2Item";				
				label: 		qsTr("Guttman's λ2 (if item dropped)");	        
				enabled: 	guttman2.checked  
			}

    		CheckBox 
    		{ 
          		id: 		lambda6item
    		  	name: 		"guttman6Item";				
    		  	label: 		qsTr("Guttman's λ6 (if item dropped)");	        
    		  	enabled: 	guttman6.checked  
    		}

			CheckBox 
			{ 
				id: 		glbItem
				name: 		"glbItem";     				
				label: 		qsTr("Greatest lower bound (if item dropped)");	
				enabled: 	glb.checked    
			}

			CheckBox 
			{ 
				id: 		plotItem
				name: 		"plotItem";     				
				label: 		qsTr("If item dropped plot");	
				enabled: 	mcdonaldItem.checked || cronbachItem.checked || lambda2Item.checked || glbItem.checked;
				
				CheckBox 
				{ 
					name: 		"orderItem";     				
					label: 		qsTr("Order items");	
					enabled: 	plotItem.checked  
				
					RadioButtonGroup 
					{
						title: 	""
						name: 	"orderType"
						
						RadioButton { value: "orderItemMean"; 	label: qsTr("Order items by mean");			checked: true	}
						RadioButton { value: "orderItemKL"; 	label: qsTr("Order items by KL-divergence")					}
						RadioButton { value: "orderItemKS"; 	label: qsTr("Order items by KS-distance")					}
					}
				}
			}
        
			CheckBox { name: "itemRestCor";						label: qsTr("Item-rest correlation")			}
			CheckBox { name: "meanItem";						label: qsTr("Mean")								}
			CheckBox { name: "sdItem";							label: qsTr("Standard deviation")				}
    	}

      Group
      {
			CheckBox 
			{
				name: 	"plotPosterior";           
				label: 	qsTr("Plot Posteriors");
			
				CheckBox 
				{   
					name: 	"fixXRange";               
					label: 	qsTr("Fix range to 0-1")
				}

				CheckBox 
				{ 
					name: 	"dispPrior";               
					label: 	qsTr("Display Priors")
				}

			}
		}

		Group
        {
			CheckBox 
            {
                id:                 probTable
                name:               "probTable"
                label:              qsTr("Probability for:")
                childrenOnSameRow:  true

                RowLayout
                {
                    DoubleField
                    {
                        name:           "probTableValueLow"
                        label:          qsTr("")
                        defaultValue:   0.70
                        min:            0
                        max:            1
                        decimals:       2
                        fieldWidth:     40
                    }

                    Label
                    {   text: qsTr("< Reliability <")   }

                    DoubleField
                    {
                        name:           "probTableValueHigh"
                        label:          qsTr("")
                        defaultValue:   .90
                        min:            0
                        max:            1
                        decimals:       2
                        fieldWidth:     40
                    }
                }
            }

			Item
			{
				width:  shadePlots.width + Theme.subOptionOffset
				height: shadePlots.height

				CheckBox 
				{ 
					id:       shadePlots
					name:     "shadePlots";              
					indent:   true
					label:    qsTr("Shade posterior region in plot"); 
					enabled:  probTable.checked    
					x:        Theme.subOptionOffset
				}
			}
		}
	}
      
	Section
	{
		title: qsTr("Convergence")
		
		Group 
		{
			title: qsTr("MCMC parameters");
			
			IntegerField
			{
				name: 			"noSamples"
				label: 			qsTr("No. samples")
				defaultValue: 	1000
				fieldWidth: 	100
				min: 			100
				max: 			1e7
			}
			
			IntegerField
			{
				name: 			"noBurnin"
				label: 			qsTr("No. burnin samples")
				defaultValue: 	50
				fieldWidth: 	100
				min: 			20
				max: 			1e6
			}
			
			IntegerField
			{
				name: 			"noThin"
				label: 			qsTr("Thinning")
				defaultValue: 	1
				fieldWidth: 	100
				min: 			1
				max: 			1e3
			}
			
			IntegerField
			{
				name: 			"noChains"
				label: 			qsTr("No. chains")
				defaultValue: 	3
				fieldWidth: 	100
				min: 			2
				max: 			100
			}
		}
		
		Group 
		{
			title: qsTr("Diagnostics")
			
			CheckBox {	name: "rHat";		label: qsTr("R-hat");		}
			CheckBox {	name: "tracePlot";	label: qsTr("Traceplots");	}
		}

		Group 
		{
			title: qsTr("Repeatability")
     
			CheckBox 
			{
				name: 				"setSeed"
				label: 				qsTr("Set seed")
				childrenOnSameRow: 	true
				
				IntegerField
				{
					name: 			"seedValue"
					label: 			""
					defaultValue: 	1234
					fieldWidth: 	100
					min: 			1
					max: 			1e9
				}
			}
		}
	}

	Section
	{
		title: qsTr("Reverse-Scaled Items")
		
		VariablesForm
		{
			height: 150
			AvailableVariablesList { name: "normalScaledItems";	 title: qsTr("Normal-Scaled Items"); source: "variables" }
			AssignedVariablesList {  name: "reverseScaledItems"; title: qsTr("Reverse-Scaled Items") }
		}
	}
	
	
	Section
	{
		title: qsTr("Missing Data Handling")
       
		RadioButtonGroup 
		{
			title: 	qsTr("Missing Values")
			name: 	"missingValues"

			RadioButton { value: "excludeCasesPairwise"; label: qsTr("Exclude cases pairwise"); checked: true}
			RadioButton { value: "excludeCasesListwise"; label: qsTr("Exclude cases listwise")}
		}
    }
}
