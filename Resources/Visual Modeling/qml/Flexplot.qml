import QtQuick			2.12
import JASP.Controls	1.0
import JASP.Widgets		1.0
import JASP				1.0

Form
{
	usesJaspResults: true

	VariablesForm
	{
		AvailableVariablesList	{ name: "allVariables" }
		AssignedVariablesList	{
				name: "dependent"		;
				title: qsTr("Dependent Variable")	;
				singleVariable: true
				onCountChanged:  nameY.value = count > 0 ? model.data(model.index(0,0)) : ""
		}
		AssignedVariablesList	{
				name: "variables"		;
				title: qsTr("Independent Variable(s)") ;
				id: varlist
				onCountChanged: {
					nameLegend.value = count > 1 ? model.data(model.index(1,0)) : "";
					nameX.value = count > 0 ? model.data(model.index(0,0)) : "";
				}

		}
		AssignedVariablesList	{
			name: "paneledVars"	;
			title: qsTr("Paneled Variable(s)");
			id: paneledVars
			onCountChanged: {
				nameCols.value = count > 0 ? model.data(model.index(0,0)) : "";
				nameRows.value = count > 1 ? model.data(model.index(1,0)) : "";
			}

		}
	}

	Section
	{
		title: qsTr("Options")

		Group
		{
			title: qsTr("<br><strong>Point controls</br></strong>")
			columns: 4
			Slider
			{
				name: "alpha"
				label: qsTr("Point transparency")
				value: 0.4
				vertical: true
				enabled: varlist.count > 0
			}
			Slider
			{
				name: "jitx"
				label: qsTr("Jitter in X")
				value: .1
				min: 0
				max: .5
				vertical: true
				enabled: varlist.count > 0
			}
			Slider
			{
				name: "jity"
				label: qsTr("Jitter in Y")
				value: 0
				min: 0
				max: .5
				vertical: true
				enabled: varlist.count > 0
			}
		}
		Group
		{
			Group
			{
				title: qsTr("<strong>Visual Statistics</strong>")
				CheckBox
				{
					name:"confidence";
					label: qsTr("Plot confidence bands")
					enabled: varlist.count > 0
				}
				DropDown
				{
					name: "type"
					values: ["Loess", "Regression", "Quadratic", "Cubic", "None"]
					label: qsTr("Fitted line (scatterplots)")
					enabled: varlist.count > 0
				}
				DropDown
				{
					name: "intervals"
					values: ["Quartiles", "Standard errors", "Standard deviations"]
					label: qsTr("Intervals (categorical predictors)")
					enabled: varlist.count > 0
				}
			}

			Group
			{
				title: qsTr("<br><strong>Other Plot Controls</strong>")
				DropDown
				{
					name: "theme"
					values: ["JASP", "Black and white", "Minimal", "Classic", "Dark"]
					label: qsTr("GGplot theme")
				}
				DropDown
				{
					name: "palette"
					values: ["GGplot Default", "Nature", "AAAS", "Lancet", "JCO", "Dark"]
					label: qsTr("Color Palette")
				}
				CheckBox
				{
					name:"bw";
					label: qsTr("Convert to grayscale");
					checked: false
				}

				CheckBox
				{
					name:"ghost";
					label: qsTr("Ghost lines");
					checked: true
					enabled: paneledVars.count > 0
				}
			}
		}
	}

	Section
	{
		title: qsTr("Plot Labels")
		Group
		{
		  title: qsTr("<br><strong>Plot Labels</strong>")
		  TextField
      {
	      id: nameX;
	      label: "X Axis Label";
	      name: "nameX";
      }
		  TextField
      {
	      id: nameY
	      label: "Y Axis Label"
	      name: "nameY";
				value: xAxis.value
      }
			TextField
      {
	      id: nameLegend
	      label: "Legend Label"
	      name: "nameLegend";
				value: legend.value
      }
			TextField
      {
	      id: nameCols
	      label: "Column Panel Label"
	      name: "nameCols";
				value: cols.value
      }
			TextField
      {
	      id: nameRows
	      label: "Row Panel Label"
	      name: "nameRows";
				value: rows.value
      }
		}
	}
}
