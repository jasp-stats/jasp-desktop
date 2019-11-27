import QtQuick 			2.8
import QtQuick.Layouts 	1.3
import JASP.Controls 	1.0
import JASP.Widgets 	1.0

Form 
{
	id: form

	TextField
	{
		label:			"Save me!"
		name:			"saveMe"
		defaultValue:	"Blablabla"
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
