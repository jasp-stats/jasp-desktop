import QtTest
import QtQuick
import JASP.Controls

// The scheduled follow-up: ComboBoxBase::bindTo's no-coercion is GENERAL
// combobox behaviour since commit cce0207fa (the multiDataSetAware() gate of C2's first pass was
// removed by Joris' amendment), so it must hold for a plain, NON-aware analysis form too: a
// bound value the DropDown cannot serve goes to the error state and stays pending - the option
// keeps the value exactly as it came in and is NEVER rewritten to entry 0 - and it binds for
// real the moment the model grows an entry that can serve it. (The aware path is pinned by
// tst_dataSetSelectionVariablesList.qml::test_unknown_saved_filter_id_waits_not_coerced.)
Item
{
	width:	1000
	height:	800

	property alias form: jaspForm

	Form
	{
		id:		jaspForm
		width:	800 // The theme of the QML test engine has no form width

		//Plain and non-aware: no dataSetSelectionOption anywhere in this form, the classic
		//FilterMenuButton world where the coercion to entry 0 used to live.
		DropDown	{ name: "method"; id: methodCombo; label: "Method"; values: ["one", "two"] }
	}

	TestCase
	{
		id:		testCase
		name:	"TestDropDownNoCoercion"

		SignalSpy
		{
			id:				spyLoader
			target:			jaspForm
			signalName:		"formCompletedSignal"
		}

		function initTestCase()
		{
			spyLoader.wait(3000)
			compare(spyLoader.count, 1, "The form should have completed")
		}

		//One function on purpose: the recovery half needs the exact pending state the first half
		//left behind, and QML test functions run alphabetically anyway.
		function test_unknown_value_pends_then_binds_when_the_model_grows()
		{
			verify(!jaspForm.analysis.multiDataSetAware, "this fixture is a plain NON-aware form, the general contract applies")

			//The classic corrupted/old-file case: the saved value simply is not one of the options.
			jaspForm.setOptions({ method: "notInOptionsYet" })
			wait(50)

			verify(methodCombo.hasError, "binding a value the model cannot serve must report the error state")
			verify(methodCombo.valueAwaitsModel, "the value must stay pending until a model can serve it")
			compare(methodCombo.currentValue, "", "and the combo shows nothing rather than a coerced entry 0")

			var bound = JSON.parse(jaspForm.createOptionsJsonAsJson(0))
			compare(String(bound.method), "notInOptionsYet", "the option keeps the value as it came in, NOT the coerced first entry (the generated Engine value of a pending combo IS the held component value - ComboBoxBase::createEngineValue, successor of the deleted mirror's storage of it)")

			//Recovery: the moment the model can serve the value, termsChangedHandler binds it for
			//real and clears the error - no rebind from the user needed.
			methodCombo.values = ["one", "two", "notInOptionsYet"]
			wait(50)

			verify(!methodCombo.hasError, "the error must clear when the option appears")
			verify(!methodCombo.valueAwaitsModel, "nothing may be left pending once it bound")
			compare(methodCombo.currentValue, "notInOptionsYet", "the pending value binds for real")

			var recovered = JSON.parse(jaspForm.createOptionsJsonAsJson(0))
			compare(String(recovered.method), "notInOptionsYet", "the option holds the very same value after the recovery")
		}
	}
}
