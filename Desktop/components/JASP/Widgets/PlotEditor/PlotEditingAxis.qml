import QtQuick				2.14
import QtQuick.Controls		2.14
import QtQuick.Layouts		1.3
import JASP.Widgets			1.0		as	JASPW
import JASP.Theme			1.0
import JASP.Controls		1.0		as	JASPC

Column
{

	id:			axis

	property var	axisModel:	null
	property string title:		""

	JASPC.TextField
	{
		id:					axisTitle
		label:				qsTr("Name of %1").arg(parent.title);
		fieldWidth:			200
		value:				!axisModel ? "" : axisModel.title
		onEditingFinished:	if(axisModel) axisModel.title = value
	}

	JASPC.DropDown
	{
		// TODO: make this work on the R side (if I've got time left)
		visible: false
		label: qsTr("Text Type")

		//Move this to C++
		values:
		[
			{ label: qsTr("Plain Text"),		value: "plainText"		},
			{ label: qsTr("R Expression"),		value: "rExpression"	},
			{ label: qsTr("LateX"),				value: "Latex"			}
		]

		indexDefaultValue: {
			var idx = ["plainText", "rExpression", "Latex"].findIndex(e => e === !axisModel ? "plainText" : axisModel.titleType);
			if (idx < 0)
			{
				console.log("Someone made a typo! titleType: '", axisModel.titleType, "' could not be found!")
				idx = 0
			}
			return idx
		}
		onCurrentIndexChanged: axisModel.titleType = currentValue
	}

	JASPC.RadioButtonGroup
	{
		id:		axisBreaksRadioButton
		title:	qsTr("Breaks")

		//define breaksType as enum
		JASPC.RadioButton { id: axisBreaksRange;		value: "range";		label:	qsTr("Specify range");		checked: if(axisModel) axisModel.breaksType === "range"		}
		JASPC.RadioButton { id: axisBreaksManual;		value: "manual";	label:	qsTr("Manually");			checked: if(axisModel) axisModel.breaksType === "manual"	}

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

		//Limits to C++ as well
		JASPC.RadioButton	{								value: "data";		label:	qsTr("Based on data");		checked: if(axisModel) axisModel.limitsType === "data"		}
		JASPC.RadioButton	{								value: "breaks";	label:	qsTr("Based on breaks");	checked: if(axisModel) axisModel.limitsType === "breaks"	}
		JASPC.RadioButton	{	id: axisLimitsManual;		value: "manual";	label:	qsTr("Set manually");		checked: if(axisModel) axisModel.limitsType === "manual"	}

		onValueChanged: axisModel.limitsType = axisLimitsRadioButton.value
	}

	JASPC.DoubleField	{	visible: axisLimitsManual.checked;	id: axisLimitsLower;	label: qsTr("lower limit");	negativeValues: true;	max: !axisModel ? 0 : axisModel.limitUpper;		defaultValue: !axisModel ? 0 : axisModel.limitLower;	onValueChanged: if(axisModel) axisModel.limitLower = value; 	}
	JASPC.DoubleField	{	visible: axisLimitsManual.checked;	id: axisLimitsUpper;	label: qsTr("upper limit");	negativeValues: true;	min: !axisModel ? 0 : axisModel.limitLower;		defaultValue: !axisModel ? 0 : axisModel.limitUpper;	onValueChanged: if(axisModel) axisModel.limitUpper = value; 	}

}
