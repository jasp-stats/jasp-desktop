import QtQuick				2.14
import QtQuick.Controls		2.14
import QtQuick.Layouts		1.3
import JASP.Widgets			1.0		as	JASPW
import JASP.Theme			1.0
import JASP.Controls		1.0		as	JASPC

/*
	TODO: it might make sense to add a global title/ box around all the options inside one axis.
 */

Column
{

	id:			axis

	property var	axisModel:	null
	property string title:		""

	JASPC.TextField
	{
		id:					axisTitle
		label:				qsTr("Title of %1").arg(parent.title);
		fieldWidth:			200
		value:				!axisModel ? "" : axisModel.title
		onEditingFinished:	if(axisModel) axisModel.title = value
	}

	JASPC.DropDown
	{
		label: qsTr("Title Type")

		//Move this to C++
		values:
		[
			{ label: qsTr("Plain Text"),		value: "character"		},
			{ label: qsTr("R Expression"),		value: "expression"		},
			{ label: qsTr("LateX"),				value: "LaTeX"			}
		]

		indexDefaultValue: {
			if (currentValue)
				console.log("current value is " + currentValue)
			else
				console.log("current value is undefined")

			if (axisModel)
				console.log("axisModel is " + axisModel.titleType)
			else
				console.log("axisModel is undefined")

			var idx;
			if (axisModel)
			{
				idx = ["character", "expression"].findIndex(e => e === axisModel.titleType);
				if (idx < 0)
				{
					console.log("Someone made a typo! titleType: '", axisModel.titleType, "' could not be found!")
					idx = 0
				}
			}
			else
				idx = 0
			console.log("idx is " + idx)
			return idx
		}
		onCurrentIndexChanged: if (axisModel) axisModel.titleType = currentValue
	}

	JASPC.RadioButtonGroup
	{
		id:		axisBreaksRadioButton
		title:	qsTr("Breaks")
		visible: axisModel ? axisModel.continuous : false

		//define breaksType as enum
		JASPC.RadioButton { id: axisBreaksRange;		value: "range";		label:	qsTr("Specify range");		checked: if(axisModel) axisModel.continuous ? axisModel.breaksType === "range"  : false	}
		JASPC.RadioButton { id: axisBreaksManual;		value: "manual";	label:	qsTr("Manually");			checked: if(axisModel) axisModel.continuous ? axisModel.breaksType === "manual" : true	}

		onValueChanged: axisModel.breaksType = axisBreaksRadioButton.value

	}

	Column
	{
		id:			breaksGroup
		spacing:	jaspTheme.columnGroupSpacing
		anchors
		{
			left:		parent.left
			right:		parent.right
			leftMargin:	jaspTheme.indentationLength
		}

		JASPC.Text
		{
			text:	qsTr("Specify breaks:")
			x:		-jaspTheme.indentationLength
		}

		JASPC.Group
		{
			JASPC.DoubleField	{	visible: axisBreaksRange.checked;	id: axisBreaksRangeFrom;	label: qsTr("from");	max: !axisModel ? 0 : axisModel.to;		defaultValue: !axisModel ? 0 : axisModel.from;		onValueChanged: if(axisModel) axisModel.from	= value;	 negativeValues: true	}
			JASPC.DoubleField	{	visible: axisBreaksRange.checked;	id: axisBreaksRangeTo;		label: qsTr("to");		min: !axisModel ? 0 : axisModel.from;	defaultValue: !axisModel ? 0 : axisModel.to;		onValueChanged: if(axisModel) axisModel.to		= value;	 negativeValues: true	}
			JASPC.DoubleField	{	visible: axisBreaksRange.checked;	id: axisBreaksRangeSteps;	label: qsTr("steps");	min: 0;									defaultValue: !axisModel ? 0 : axisModel.steps;		onValueChanged: if(axisModel) axisModel.steps	= value;								}
		}

		PlotEditingAxisTable
		{
			visible:		axisBreaksManual.checked
			model:			axisModel
			width:			breaksGroup.width
		}
	}

	JASPC.RadioButtonGroup
	{
		id:		axisLimitsRadioButton
		name:	"axisLimits";
		title:	qsTr("Limits")
		visible: axisModel ? axisModel.continuous : false

		//Limits to C++ as well
		JASPC.RadioButton	{								value: "data";		label:	qsTr("Based on data");		checked: if(axisModel) axisModel.limitsType === "data"		}
		JASPC.RadioButton	{								value: "breaks";	label:	qsTr("Based on breaks");	checked: if(axisModel) axisModel.limitsType === "breaks"	}
		JASPC.RadioButton	{	id: axisLimitsManual;		value: "manual";	label:	qsTr("Set manually");		checked: if(axisModel) axisModel.limitsType === "manual"	}

		onValueChanged: axisModel.limitsType = axisLimitsRadioButton.value
	}

	JASPC.DoubleField	{	visible: axisLimitsManual.checked;	id: axisLimitsLower;	label: qsTr("Lower limit");	negativeValues: true;	max: !axisModel ? 0 : axisModel.limitUpper;		defaultValue: !axisModel ? 0 : axisModel.limitLower;	onValueChanged: if(axisModel) axisModel.limitLower = value; 	}
	JASPC.DoubleField	{	visible: axisLimitsManual.checked;	id: axisLimitsUpper;	label: qsTr("Upper limit");	negativeValues: true;	min: !axisModel ? 0 : axisModel.limitLower;		defaultValue: !axisModel ? 0 : axisModel.limitUpper;	onValueChanged: if(axisModel) axisModel.limitUpper = value; 	}

}
