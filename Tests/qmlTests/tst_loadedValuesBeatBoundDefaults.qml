import QtTest
import QtQuick
import JASP.Controls

// A loaded option value (a reopened JASP file, R syntax, jaspTools) is what the user chose: a control
// whose default is a QML binding on another control must keep it, also when that other control is
// bound after it and its binding re-evaluates. The three shapes such a default takes (all seen in
// jaspDescriptives' Raincloud Plots):
//   . CheckBox		checked:			secondaryBD.count === 0
//   . DoubleField	defaultValue:		(secondaryBD.count === 0) ? 0.1 : 0.2
//   . DropDown		indexDefaultValue:	showBoxBD.checked ? 0 : 2		(an alias of the combo's currentIndex)
// Without a loaded value the default must still follow its binding.
// And the option generated for the engine is the value the control holds, also for a DropDown with a startValue.
// Fixture (Tests/testqml.cpp): TestInts (ordinal), TestLetters (nominal), TestDoubles (scale), TestNominal (nominal).
Item
{
	width:	1000
	height:	1200

	property alias form: jaspForm

	Form
	{
		id:		jaspForm
		width:	800 // The theme of the QML test engine has no form width

		VariablesForm
		{
			AvailableVariablesList	{ name: "allVarsBD" }
			AssignedVariablesList	{ id: secondaryBD;	name: "secondaryBD";	singleVariable: true }
		}

		CheckBox	{ id: colorBD;		name: "colorBD";	checked: secondaryBD.count === 0 }
		CheckBox	{ id: legendBD;		name: "legendBD";	checked: secondaryBD.count === 1 }
		CheckBox	{ id: followBD;		name: "followBD";	checked: secondaryBD.count === 0 } // never given a value by these tests
		DoubleField	{ id: widthBD;		name: "widthBD";	defaultValue: (secondaryBD.count === 0) ? 0.1 : 0.2 }
		CheckBox	{ id: showBoxBD;	name: "showBoxBD";	checked: true }
		DropDown	{ id: paletteBD;	name: "paletteBD";	startValue: "second";	values: ["first", "second", "third"] }
		DropDown	{ id: outlineBD;	name: "outlineBD";	indexDefaultValue: showBoxBD.checked ? 0 : 2;	values: ["first", "second", "third"] }
		DropDown
		{
			id:					quantilesBD
			name:				"quantilesBD"
			indexDefaultValue:	6
			values:				[ {label: "1", value: 1}, {label: "2", value: 2}, {label: "3", value: 3}, {label: "4", value: 4}, {label: "5", value: 5},
								  {label: "6", value: 6}, {label: "7", value: 7}, {label: "8", value: 8}, {label: "9", value: 9} ]
		}
	}

	TestCase
	{
		name: "TestLoadedValuesBeatBoundDefaults"

		SignalSpy
		{
			id:			spyLoader
			target:		jaspForm
			signalName:	"formCompletedSignal"
		}

		function initTestCase()
		{
			spyLoader.wait(3000)
			compare(spyLoader.count, 1, "The form should have completed")
		}

		// Every test starts from the defaults, so that the dependency of a bound default changes WHILE the
		// options of the test are loaded (QML test functions run alphabetically and share this form).
		function resetForm()
		{
			jaspForm.setOptions({ secondaryBD: { value: "", types: [] }, colorBD: true, legendBD: false, widthBD: 0.1, showBoxBD: true, outlineBD: "first", quantilesBD: 7, paletteBD: "second" })
			wait(50)
		}

		function loaded()
		{
			wait(50) // let queued writes settle
			return JSON.parse(jaspForm.createOptionsJsonAsJson(0))
		}

		function test_checkBoxKeepsLoadedValueAgainstItsBinding()
		{
			// Without a secondary variable the binding says true: the loaded false must stay
			resetForm()
			jaspForm.setOptions({ secondaryBD: { value: "", types: [] }, colorBD: false })
			compare(loaded().colorBD, false, "colorBD keeps the loaded false")

			// With a secondary variable the binding says true for legendBD: the loaded false must stay
			resetForm()
			jaspForm.setOptions({ secondaryBD: { value: "TestLetters", types: ["nominal"] }, legendBD: false })
			compare(loaded().legendBD, false, "legendBD keeps the loaded false")
		}

		function test_defaultWithoutLoadedValueStillFollowsItsBinding()
		{
			resetForm()
			compare(followBD.checked, true, "no secondary variable: followBD is checked by its binding")
			jaspForm.setOptions({ secondaryBD: { value: "TestLetters", types: ["nominal"] } })
			wait(50)
			compare(followBD.checked, false, "followBD got no loaded value, so it follows its binding")
			resetForm()
			compare(followBD.checked, true, "and back")
		}

		function test_doubleFieldKeepsLoadedValueWhenItsDefaultChanges()
		{
			resetForm()
			jaspForm.setOptions({ secondaryBD: { value: "TestLetters", types: ["nominal"] }, widthBD: 0.1 })
			compare(loaded().widthBD, 0.1, "widthBD keeps the loaded 0.1 although its default became 0.2")
		}

		function test_dropDownWithStartValueGeneratesItsValue()
		{
			resetForm()
			compare(loaded().paletteBD, "second", "without another value the startValue is the value")
			jaspForm.setOptions({ paletteBD: "third" })
			compare(paletteBD.currentValue, "third", "the DropDown shows the loaded value")
			compare(loaded().paletteBD, "third", "and the engine gets it, not the startValue")
		}

		function test_dropDownKeepsLoadedValueAgainstItsIndexBinding()
		{
			resetForm()
			jaspForm.setOptions({ showBoxBD: false, outlineBD: "first" })
			compare(loaded().outlineBD, "first", "outlineBD keeps the loaded value, not the default of the bound index")
		}

		function test_dropDownKeepsLoadedNumericValue()
		{
			resetForm()
			jaspForm.setOptions({ quantilesBD: 3 })
			compare(Number(loaded().quantilesBD), 3, "quantilesBD keeps the loaded 3, not its default 7")
		}
	}
}
