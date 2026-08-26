import QtQuick
import QtTest
import JASP

TestCase
{
	name:		"TestScriptConstructor"
	width:		900
	height:		600
	when:		windowShown

	ScriptConstructor
	{
		id:		sc
		mode:	ScriptConstructor.Filter
		width:	900
		height:	600
	}

	// In the headless test there is no ColumnsModel, so column types resolve to the
	// scale fallback. The exact per-type R output is covered by the golden tests in
	// testall.cpp which use a real column-type provider.

	function test_load_json_generates_r()
	{
		sc.constructorJson = '{"formulas":[{"nodeType":"Operator","operator":">","leftArgument":{"nodeType":"Column","columnName":"TestInts","columnTypeUser":-1,"columnTypeDrop":-1},"rightArgument":{"nodeType":"Number","value":2}}]}'

		compare(sc.rCode, "(TestInts.scale > 2)\n")
		compare(sc.somethingChanged, false)
		compare(sc.jsonChanged(), false)
		compare(sc.checkAndApply(), true)
		compare(sc.lastCheckPassed, true)
	}

	function test_check_and_apply_emits()
	{
		sc.constructorJson = '{"formulas":[{"nodeType":"Operator","operator":"==","leftArgument":{"nodeType":"Column","columnName":"TestLetters","columnTypeUser":-1,"columnTypeDrop":-1},"rightArgument":{"nodeType":"String","text":"A"}}]}'

		var appliedR = ""
		var handler = function(json, rCode) { appliedR = rCode; }
		sc.applyRequested.connect(handler)

		compare(sc.checkAndApply(), true)
		compare(appliedR, "(TestLetters.scale == 'A')\n")

		sc.applyRequested.disconnect(handler)
	}

	function test_incomplete_formula_fails_check()
	{
		sc.constructorJson = '{"formulas":[{"nodeType":"Operator","operator":"+","leftArgument":{"nodeType":"Column","columnName":"TestInts","columnTypeUser":-1,"columnTypeDrop":-1},"rightArgument":null}]}'

		compare(sc.checkAndApply(), false)
		compare(sc.lastCheckPassed, false)
	}

	function test_non_boolean_root_fails_filter_check()
	{
		sc.constructorJson = '{"formulas":[{"nodeType":"Operator","operator":"+","leftArgument":{"nodeType":"Number","value":1},"rightArgument":{"nodeType":"Number","value":2}}]}'

		compare(sc.checkAndApply(), false)
	}

	function test_empty_filter_applies()
	{
		sc.constructorJson = '{"formulas":[]}'
		compare(sc.checkAndApply(), true)
		compare(sc.rCode, "")
	}
}
