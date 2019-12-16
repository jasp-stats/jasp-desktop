import QtQuick 			2.8
import QtQuick.Layouts 	1.3
import JASP.Controls 	1.0
import JASP.Widgets 	1.0

Form 
{
	TextField
	{
		label:			"Save me!"
		name:			"saveMe"
		value:			"Blablabla"
	}

	CheckBox
	{
		label:		"A checkbox!"
		name: 		"checkbox_0"
		checked: 	false
	}
	
	CheckBox
	{
		label:		"Another checkbox!"
		name: 		"checkbox_1"
		checked: 	false
	}
}
