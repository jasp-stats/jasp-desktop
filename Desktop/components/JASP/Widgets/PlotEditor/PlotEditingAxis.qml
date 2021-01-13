import QtQuick				2.14
import QtQuick.Controls		2.15
import QtQuick.Layouts		1.3
import JASP.Widgets			1.0		as	JASPW
import JASP.Theme			1.0
import JASP.Controls		1.0		as	JASPC
import JASP.PlotEditor		1.0

/*
	TODO: it might make sense to add a global title/ box around all the options inside one axis.
 */

Column
{
					id:			axis
					spacing:	jaspTheme.columnGroupSpacing
	property var	axisModel:	null
	property string title:		""
	property bool	advanced:	false


	JASPC.TextField
	{
		id:					axisTitle
		label:				qsTr("Title of %1").arg(axis.title);
		fieldWidth:			200
		value:				axisModel.title
		onEditingFinished:	if(axisModel) axisModel.title = value
		enabled:			advanced || axisModel.titleType === parseInt(AxisModel.TitleCharacter)

		ToolTip
		{
			id:			axisTitleToolTip
			enabled:	!parent.enabled
			visible:	!axisTitle.enabled && axisTitle.hovered
			delay:		0
			timeout:	5000
			text:		qsTr("The title can only be modified in advanced mode because it is not plain text.");
		}
	}


	JASPC.DropDown
	{
		id:		titleTypeDropDown
		label: qsTr("Title Type")

		values:
		[
			{ label: qsTr("Plain Text"),		value: AxisModel.TitleCharacter		},
			{ label: qsTr("R Expression"),		value: AxisModel.TitleExpression	},
			{ label: qsTr("LateX"),				value: AxisModel.TitleLaTeX			}
		]

		startValue: axisModel.titleType

		onCurrentValueChanged: axisModel.titleType = parseInt(currentValue)
		visible: advanced
	}

	JASPC.RadioButtonGroup
	{
		id:		axisBreaksRadioButton
		title:	qsTr("Breaks")
		visible: axisModel.continuous

		//define breaksType as enum
		JASPC.RadioButton { id: axisBreaksRange;		value: "range";		label:	qsTr("Specify range");		checked: if(axisModel) axisModel.continuous ? advanced || axisModel.breaksType === AxisModel.BreaksRange  : false	}
		JASPC.RadioButton { id: axisBreaksManual;		value: "manual";	label:	qsTr("Manually");			checked: if(axisModel) axisModel.continuous ? advanced || axisModel.breaksType === AxisModel.BreaksManual : true	}

		onValueChanged: axisModel.breaksType = (axisBreaksRadioButton.value === "range" ? AxisModel.BreaksRange : AxisModel.BreaksManual)
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
			text:	qsTr("Specify ticks:")
			x:		-jaspTheme.indentationLength
		}

		JASPC.Group
		{
			visible:	 axisBreaksRange.checked

			JASPC.DoubleField
			{
				id: axisBreaksRangeFrom;	label: qsTr("minimum");	value:	axisModel.from;	negativeValues: true
				onValueChanged:
				{
					if(axisModel)
					{
						axisModel.from = value;
						if (!advanced)
						{
							axisModel.limitLower = value;
							if (axisModel.limitLower === axisModel.from) // eases transition to advanced mode
								axisModel.limitsType = AxisModel.LimitsBreaks
						}
					}
				}
			}
			JASPC.DoubleField
			{
				id: axisBreaksRangeTo;	label: qsTr("maximum");	value:	axisModel.to;	negativeValues: true
				onValueChanged:
				{
					if(axisModel)
					{
						axisModel.to = value;
						if (!advanced)
						{
							axisModel.limitUpper = value;
							if (axisModel.limitLower === axisModel.from) // eases transition to advanced mode
								axisModel.limitsType = AxisModel.LimitsBreaks
						}
					}
				}
			}
			JASPC.DoubleField	{	id: axisBreaksRangeSteps;	label: qsTr("stepsize");	value:	axisModel.steps;	onValueChanged: if(axisModel) axisModel.steps	= value;								}
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
		visible:	advanced && axisModel.continuous

		//Limits to C++ as well
		JASPC.RadioButton	{									value: "data";		label:	qsTr("Based on data");		checked: if(axisModel) axisModel.limitsType === AxisModel.LimitsData	}
		JASPC.RadioButton	{									value: "breaks";	label:	qsTr("Based on ticks");		checked: if(axisModel) axisModel.limitsType === AxisModel.LimitsBreaks	}
		JASPC.RadioButton	{	id: axisLimitsManual;			value: "manual";	label:	qsTr("Set manually");		checked: if(axisModel) axisModel.limitsType === AxisModel.LimitsManual	}

		onValueChanged: axisModel.limitsType = (axisLimitsRadioButton.value === "data" ? AxisModel.LimitsData : axisLimitsRadioButton.value === "breaks" ? AxisModel.LimitsBreaks : AxisModel.LimitsManual)
	}

	JASPC.DoubleField	{	visible: advanced && axisLimitsManual.checked;	id: axisLimitsLower;	label: qsTr("Lower limit");	negativeValues: true;	max: axisModel.limitUpper;		value: axisModel.limitLower;	onValueChanged: if(axisModel) axisModel.limitLower = value; 	}
	JASPC.DoubleField	{	visible: advanced && axisLimitsManual.checked;	id: axisLimitsUpper;	label: qsTr("Upper limit");	negativeValues: true;	min: axisModel.limitLower;		value: axisModel.limitUpper;	onValueChanged: if(axisModel) axisModel.limitUpper = value; 	}
}
