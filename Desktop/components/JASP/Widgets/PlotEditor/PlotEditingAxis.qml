import QtQuick				2.14
import QtQuick.Controls		2.14
import QtQuick.Layouts		1.3
import JASP.Widgets			1.0		as	JASPW
import JASP.Theme			1.0
import JASP.Controls		1.0		as	JASPC
import JASP.PlotEditorAxis	1.0

/*
	TODO: it might make sense to add a global title/ box around all the options inside one axis.


	Naming scheme (maybe this should just go in some documentation file):

	JASP/ user				|			Internal name				|			ggplot2
	-------------------------------------------------------------------------------------------
	Title of ...			|			axisTitle					|			name
	Title type				|			titleType					|			NA
	Ticks					|			breaks						|			breaks
	Ticks type				|			breaksType					|			NA
	Range					|			limits						|			limits

	There are also labels, but those can only be edited in the table.

 */

Column
{
					id:			axis
					spacing:	jaspTheme.columnGroupSpacing
	property var	axisModel:	null
	property string title:		""

	JASPC.TextField
	{
		id:					axisTitle
		label:				qsTr("Title of %1").arg(axis.title);
		fieldWidth:			200
		value:				axisModel.title
		onEditingFinished:	if(axisModel) axisModel.title = value
		enabled:			plotEditorModel.advanced || axisModel.titleType === parseInt(AxisModel.TitleCharacter)

		// TODO: does not work!
		toolTip:			plotEditorModel.advanced ? "test" : qsTr("The title can only be modified in advanced mode because it is not plain text.");

	}


	JASPC.DropDown
	{
		id:		titleTypeDropDown
		label: qsTr("Title Type")

		values:
		[
			{ label: qsTr("Plain text"),		value: AxisModel.TitleCharacter		},
			{ label: qsTr("R expression"),		value: AxisModel.TitleExpression	},
//			{ label: qsTr("LateX"),				value: AxisModel.TitleLaTeX			},		<-- maybe later, see https://github.com/stefano-meschiari/latex2exp
			{ label: qsTr("Hide title"),		value: AxisModel.TitleNull			}
		]

		startValue: axisModel.titleType

		onCurrentValueChanged: axisModel.titleType = parseInt(currentValue)
		visible: plotEditorModel.advanced
	}

	JASPC.RadioButtonGroup
	{
		id:		axisBreaksRadioButton
		title:	qsTr("Ticks")
		visible: axisModel.continuous

		JASPC.RadioButton { id: axisBreaksRange;	value: "range";		label:	qsTr("Specify sequence");	checked: if(axisModel) axisModel.continuous ? plotEditorModel.advanced || axisModel.breaksType === AxisModel.BreaksRange	: false										}
		JASPC.RadioButton { id: axisBreaksManual;	value: "manual";	label:	qsTr("Manually");			checked: if(axisModel) axisModel.continuous ? plotEditorModel.advanced || axisModel.breaksType === AxisModel.BreaksManual	: true	;	visible: axisModel.continuous;	}
		JASPC.RadioButton { id: axisBreaksNull;		value: "NULL";		label:	qsTr("Hide ticks");			checked: if(axisModel) axisModel.continuous ? plotEditorModel.advanced || axisModel.breaksType === AxisModel.BreaksNull		: false										}

		onValueChanged:
		{
			switch	(axisBreaksRadioButton.value)
			{
				case "range":	axisModel.breaksType = AxisModel.BreaksRange;		break;
				case "manual":	axisModel.breaksType = AxisModel.BreaksManual;		break;
				case "NULL":	axisModel.breaksType = AxisModel.BreaksNull;		break;
			}
		}
	}

	Column
	{
		id:			breaksGroup
		visible:	axisModel.continuous && !axisBreaksNull.checked
		spacing:	jaspTheme.columnGroupSpacing
		anchors
		{
			left:		parent.left
			right:		parent.right
			leftMargin:	jaspTheme.indentationLength
		}

		JASPC.Text
		{
			text:	qsTr("Specify ticks:")
			x:		-jaspTheme.indentationLength
		}

		JASPC.Group
		{
			visible:	 axisBreaksRange.checked

			JASPC.DoubleField	{	id: axisBreaksRangeFrom;	label: qsTr("from");	value: 	axisModel.from;		onValueChanged: if(axisModel) axisModel.from	= value;	negativeValues: true	}
			JASPC.DoubleField	{	id: axisBreaksRangeTo;		label: qsTr("to");		value:	axisModel.to;		onValueChanged: if(axisModel) axisModel.to		= value;	negativeValues: true	}
			JASPC.DoubleField	{	id: axisBreaksRangeSteps;	label: qsTr("steps");	value:	axisModel.steps;	onValueChanged: if(axisModel) axisModel.steps	= value;							}
		}

		PlotEditingAxisTable
		{
			visible:			axisBreaksManual.checked
			model:				axisModel
			implicitWidth:		breaksGroup.width
		}
	}

	JASPC.RadioButtonGroup
	{
		id:			axisLimitsRadioButton
		name:		"axisLimits";
		title:		qsTr("Range")
		visible:	plotEditorModel.advanced && axisModel.continuous

		JASPC.RadioButton	{							value: "data";		label:	qsTr("Based on data");		checked: if(axisModel) axisModel.limitsType === AxisModel.LimitsData	}
		JASPC.RadioButton	{	id: axisLimitsBreaks;	value: "breaks";	label:	qsTr("Based on ticks");		checked: if(axisModel) axisModel.limitsType === AxisModel.LimitsBreaks;		enabled: !axisBreaksNull.checked	}
		JASPC.RadioButton	{	id: axisLimitsManual;	value: "manual";	label:	qsTr("Set manually");		checked: if(axisModel) axisModel.limitsType === AxisModel.LimitsManual	}

		onValueChanged: axisModel.limitsType = (axisLimitsRadioButton.value === "data" ? AxisModel.LimitsData : axisLimitsRadioButton.value === "breaks" ? AxisModel.LimitsBreaks : AxisModel.LimitsManual)
	}

	JASPC.DoubleField	{	visible: plotEditorModel.advanced && axisLimitsManual.checked;	id: axisLimitsLower;	label: qsTr("Lower limit");	negativeValues: true;	max: axisModel.limitUpper;		value: axisModel.limitLower;	onValueChanged: if(axisModel) axisModel.limitLower = value; 	}
	JASPC.DoubleField	{	visible: plotEditorModel.advanced && axisLimitsManual.checked;	id: axisLimitsUpper;	label: qsTr("Upper limit");	negativeValues: true;	min: axisModel.limitLower;		value: axisModel.limitUpper;	onValueChanged: if(axisModel) axisModel.limitUpper = value; 	}
}
