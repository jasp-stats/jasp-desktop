//This file serves as both styleguide and example of it for QML forms to be used in modules for JASP.
//Comments (like this) will describe what the "approved" style is and how to use it.
//Make sure you indent using tabs and *not* with spaces. The tabs should have a 4-space width.

import QtQuick 			2.8
import QtQuick.Layouts 	1.3
import JASP.Controls 	1.0
import JASP.Widgets 	1.0

//Not exactly a style thing but every QML form should have Form as it's root:
Form 
{				//Opening braces should on their own line and at the exact same indent as the closing brace.

	GroupBox	//A subitem is indented and always has a space between the previous subitem/property/function
	{
		title:		qsTr("A nice grouping of elements") //putting qsTr(...) around string constants allows us to make the interface translatable.
		
		IntegerField
		{	
			//If you have multiple properties then place each on their own line.
			//You should also make sure that the values start at the same indent, this greatly improves readability!
			id:				theRightWayForSpecifyingProperties
			name:			"someInteger"
			defaultValue:	1
			min:			0
			max:			123
			text:			qsTr("You can enter an integer here: ")
			fieldWidth:		100
			toolTip:		qsTr("This is an incredibly useful tooltip right?")
		}

		/*	There is an exception to the above rules on properties and it plays into situations such as below.
			Here we have a collection of similar elements (CheckBox) that have a rather limited amount of properties to be set.
			If we would spread them out like you should normally do it would simply take up a lot of space and not be more readable.
			So in this case we make one-liners of them and we align the properties so that they line up nicely. */
		Group
		{
			id:	someCheckBoxes

			CheckBox { name: "checkBoxA";	label: qsTr("The first checkbox"); checked: true	}	
			CheckBox { name: "checkBoxB";	label: qsTr("The second checkbox")					}
			CheckBox { name: "checkBoxC";	label: qsTr("The last checkbox")					}
		}

		//Another common occurence of this style is for variable lists:
		VariablesForm
		{
			AvailableVariablesList	{ name: "availbleVariables"								}
			AssignedVariablesList	{ name: "variables";		title: qsTr("Variables")	}
			AssignedVariablesList	{ name: "splitby";			title: qsTr("Split");		singleVariable: true; suggestedColumns: ["ordinal", "nominal"] }
		}

		//But in most cases you should follow the style documented at "theRightWayForSpecifyingProperties"
	}
}	//Closing braces get their own line just like their opening counterpart
