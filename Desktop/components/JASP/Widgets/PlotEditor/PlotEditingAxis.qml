import QtQuick				2.14
import QtQuick.Controls		2.14
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

	JASPC.TextField
	{
		id:					axisTitle
		label:				qsTr("Title of %1").arg(axis.title);
		fieldWidth:			200
		value:				axisModel.title
		onEditingFinished:	if(axisModel) axisModel.title = value
	}



	JASPC.DropDown
	{
		id:		titleTypeDropDown
		label: qsTr("Title Type")

		//Ok, this dropdowncode looks horrible but Bruno might be able to help out with making this less weird I hope
		Connections
		{
			target:				axisModel
			onTitleTypeChanged:	titleTypeDropDown.handleTitleType()

		}

		function handleTitleType()
		{
			if(axisModel.titleType === AxisModel.TitleCharacter)	titleTypeDropDown.currentIndex = 0;
			if(axisModel.titleType === AxisModel.TitleExpression)	titleTypeDropDown.currentIndex = 1;
			if(axisModel.titleType === AxisModel.TitleLaTeX)		titleTypeDropDown.currentIndex = 2;
		}

		values:
		[
			{ label: qsTr("Plain Text"),		value: "?"		}, //What is the value here anyway? What would we use it for? Is it only for R?
			{ label: qsTr("R Expression"),		value: "??"		},
			{ label: qsTr("LateX"),				value: "???"	}
		]

		indexDefaultValue:		0
		onCurrentIndexChanged:	if (axisModel)
		{
			if(currentIndex === 0)	axisModel.titleType = AxisModel.titleCharacter
			if(currentIndex === 1)	axisModel.titleType = AxisModel.TitleExpression
			if(currentIndex === 2)	axisModel.titleType = AxisModel.TitleLaTeX
		}
	}

	JASPC.RadioButtonGroup
	{
		id:		axisBreaksRadioButton
		title:	qsTr("Breaks")
		visible: axisModel.continuous

		//define breaksType as enum
		JASPC.RadioButton { id: axisBreaksRange;		value: "range";		label:	qsTr("Specify range");		checked: if(axisModel) axisModel.continuous ? axisModel.breaksType === AxisModel.BreaksRange  : false	}
		JASPC.RadioButton { id: axisBreaksManual;		value: "manual";	label:	qsTr("Manually");			checked: if(axisModel) axisModel.continuous ? axisModel.breaksType === AxisModel.BreaksManual : true	}

		onValueChanged: axisModel.breaksType = (axisBreaksRadioButton.value === "range" ? AxisModel.BreaksRange : AxisModel.BreaksManual)
	}

	Column
	{
		id:			breaksGroup
		spacing:	jaspTheme.columnGroupSpacing
		//visible:	axisModel.hasBreaks this property is a little misleading so turned off for now
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
			visible:	 axisBreaksRange.checked

			JASPC.DoubleField	{	id: axisBreaksRangeFrom;	label: qsTr("from");	/*defaultValue: axisModel.from;	*/	value: 	axisModel.from;		onValueChanged: if(axisModel) axisModel.from	= value;		negativeValues: true	}
			JASPC.DoubleField	{	id: axisBreaksRangeTo;		label: qsTr("to");		/*defaultValue: axisModel.to;	*/	value:	axisModel.to;		onValueChanged: if(axisModel) axisModel.to		= value;		negativeValues: true	}
			JASPC.DoubleField	{	id: axisBreaksRangeSteps;	label: qsTr("steps");	/*defaultValue: axisModel.steps;*/	value:	axisModel.steps;	onValueChanged: if(axisModel) axisModel.steps	= value;								}
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
		title:		qsTr("Limits")
		visible:	axisModel.continuous

		//Limits to C++ as well
		JASPC.RadioButton	{									value: "data";		label:	qsTr("Based on data");		checked: if(axisModel) axisModel.limitsType === AxisModel.LimitsData		}
		JASPC.RadioButton	{	visible: axisModel.haBreaks;	value: "breaks";	label:	qsTr("Based on breaks");	checked: if(axisModel) axisModel.limitsType === AxisModel.LimitsBreaks	}
		JASPC.RadioButton	{	id: axisLimitsManual;			value: "manual";	label:	qsTr("Set manually");		checked: if(axisModel) axisModel.limitsType === AxisModel.LimitsManual	}

		onValueChanged: axisModel.limitsType = (axisLimitsRadioButton.value === "data" ? AxisModel.LimitsData : axisLimitsRadioButton.value === "breaks" ? AxisModel.LimitsBreaks : AxisModel.LimitsManual)
	}

	JASPC.DoubleField	{	visible: axisLimitsManual.checked;	id: axisLimitsLower;	label: qsTr("Lower limit");	negativeValues: true;	max: axisModel.limitUpper;		value: axisModel.limitLower;	onValueChanged: if(axisModel) axisModel.limitLower = value; 	}
	JASPC.DoubleField	{	visible: axisLimitsManual.checked;	id: axisLimitsUpper;	label: qsTr("Upper limit");	negativeValues: true;	min: axisModel.limitLower;		value: axisModel.limitUpper;	onValueChanged: if(axisModel) axisModel.limitUpper = value; 	}
}
