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
import JASP.Theme		1.0
import JASP.Widgets 	1.0

Form
{
  VariablesForm
  {
    height: 300
    AvailableVariablesList 	{ name: "allVariablesList" }		
    AssignedVariablesList 	{ name: "variables"; title: qsTr("Variables")}
  }
  
  Section
  {
    title: 	qsTr("Single-Test Reliability")

    Group
    {
      title: qsTr("Scale Statistics")

      CheckBox 
	  {
        name: 				"intervalOn"
        label:				qsTr("Confidence Interval")
        checked: 			true
        childrenOnSameRow: 	true

        CIField 
        {      
          name: 		"confidenceIntervalValue";   
          label: 		"";
          defaultValue: 95;
        }
      }

      CheckBox 
      {    
        id:     	mcdonaldf
        name:   	"mcDonaldScalef"	
        label:  	qsTr("McDonald's ω")         
        checked: 	true
      }
      
	  CheckBox 
      {     
        name: 	"alphaScalef";				
        label: 	qsTr("Cronbach's α");         
        id: 	cronbachf 
      }

      CheckBox 
      {     
        id: 	guttman2f       	          
        name: 	"guttman2Scalef";			
        label: 	qsTr("Guttman's λ2");         
      }

      CheckBox 
      {     
        id: 	guttman6f       	          
        name: 	"guttman6Scalef";			
        label: 	qsTr("Guttman's λ6");         
      }

      CheckBox 
      {     
        id: 	glbf      	              
        name: 	"glbScalef";				  
        label: 	qsTr("Greatest lower bound"); 
      }

      CheckBox { name: "averageInterItemCor";	label: qsTr("Average interitem correlation")	}
      CheckBox { name: "meanScale";				label: qsTr("Mean")							}
      CheckBox { name: "sdScale";				label: qsTr("Standard deviation")			}
      
    }
    
    Group
    {
      title: qsTr("Individual Item Statistics")

      CheckBox 
      { 
        name: 		"mcDonaldItemf";				
        label: 		qsTr("McDonald's ω  (if item dropped)");	        
        enabled: 	mcdonaldf.checked 
      }

      CheckBox 
      { 
        name: 		"alphaItemf";				
        label: 		qsTr("Cronbach's α (if item dropped)");	     
        enabled: 	cronbachf.checked 
      }

      CheckBox 
      { 
        name: 		"guttman2Itemf";			
        label: 		qsTr("Guttman's λ2 (if item dropped)");	       
        enabled: 	guttman2f.checked  
      }

      CheckBox 
      { 
        name: 		"guttman6Itemf";			
        label: 		qsTr("Guttman's λ6 (if item dropped)");	       
        enabled: 	guttman6f.checked  
      }
      
	  CheckBox 
      { 
        name: 		"glbItemf";     		
        label: 		qsTr("Greatest lower bound (if item dropped)");	
        enabled: 	glbf.checked     
      }    
      
	  CheckBox { name: "itemRestCor";	label: qsTr("Item-rest correlation")				}
      CheckBox { name: "meanItem";		label: qsTr("Mean")								}
      CheckBox { name: "sdItem";		label: qsTr("Standard deviation")				}
    }
  }

  Section
  {
    title: qsTr("Reverse-Scaled Items")
    
    VariablesForm
    {
      height: 150

      AvailableVariablesList { name: "normalScaledItems";	 title: qsTr("Normal-Scaled Items"); source: "variables" }
      AssignedVariablesList {  name: "reverseScaledItems";	 title: qsTr("Reverse-Scaled Items") }
    }
  }
  
  Section
  {
    title: qsTr("Advanced Options")

    RadioButtonGroup 
	{
        title: 	qsTr("Missing Values")
        name: 	"missingValuesf"

        RadioButton { value: "excludeCasesPairwise"; label: qsTr("Exclude cases pairwise"); checked: true}
        RadioButton { value: "excludeCasesListwise"; label: qsTr("Exclude cases listwise")}
    } 
    
	Group 
	{
      title: qsTr("Bootstrap")
      
	  IntegerField
      {
        name: 			"noSamplesf"
        label: 			qsTr("No. of bootstrap samples")
        defaultValue: 	1000
        fieldWidth: 	50
        min: 			100
        max: 			1e7
      }
      
	  RadioButtonGroup 
	  {
        title:	""
        name: 	"bootType"

        RadioButton {value: "bootNonpara"; label: qsTr("Non-parametric bootstrap"); checked: true}
        RadioButton {value: "bootPara"; label: qsTr("Parametric bootstrap")}
      }

    }    

    RadioButtonGroup 
	{
      title: 	qsTr("McDonald's ω Estimation")
      name: 	"omegaEst"

      RadioButton 
	  {
		  value: 	"cfa"
		  label: 	qsTr("CFA")
		  checked: 	true

          CheckBox
          {
            name:     "fitMeasures"	
            label:    qsTr("Single Factor Model Fit")         
            enabled:  mcdonaldf.checked
          }

          RadioButtonGroup 
		  {
            title: 	qsTr("Interval")
            name: 	"omegaInterval"

            RadioButton
			{
            
              value:	"omegaAnalytic"	
              label:  	qsTr("Analytic interval")         
              checked: 	true
            }
            
			RadioButton
            {
              value: 	"omegaBoot"	
              label:  	qsTr("Bootstrapped interval")         
            }
          }
        }
        RadioButton { value: "pfa"; label: qsTr("PFA")}
    }
    
    Group
	{
      title: qsTr("Cronbach's α Estimation")
      
        RadioButtonGroup 
		{
        title:	""
        name: 	"alphaMethod"
        
		RadioButton
		{
          value:   	"alphaUnstand"	
          label:   	qsTr("Unstandardized")         
          checked:	true
        }
        
		RadioButton
        {
          value:  	"alphaStand"	
          label:  	qsTr("Standardized")         
        }
      }
      
      RadioButtonGroup 
	  {
        title: 	qsTr("Interval")
        name: 	"alphaInterval"

        RadioButton
		{
          value:  	"alphaAnalytic"	
          label:  	qsTr("Analytic interval")         
          checked: 	true
        }

        RadioButton
        {
          value: 	"alphaBoot"	
          label:  	qsTr("Bootstrapped interval")         
        }
      }
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
}
