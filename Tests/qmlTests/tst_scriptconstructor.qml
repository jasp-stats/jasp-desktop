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
		mode:	ScriptConstructorMode.Filter
		width:	900
		height:	600
	}

	ScriptConstructor
	{
		id:					scHidden
		mode:				ScriptConstructorMode.Filter
		width:				500
		height:				400
		visible:			false
		deferUntilVisible:	true
	}

	// In the headless test there is no ColumnsModel, so column types resolve to the
	// scale fallback. The exact per-type R output is covered by the golden tests in
	// testall.cpp which use a real column-type provider.

	// Two complete boolean formulas joined with `&` (valid in filter mode, invalid as a
	// computed column): 2 > 3 and 1 < 5.
	readonly property string twoBooleanFormulas: '{"formulas":[{"nodeType":"Operator","operator":">","leftArgument":{"nodeType":"Number","value":2},"rightArgument":{"nodeType":"Number","value":3}},{"nodeType":"Operator","operator":"<","leftArgument":{"nodeType":"Number","value":1},"rightArgument":{"nodeType":"Number","value":5}}]}'

	function test_deferred_build_on_visible()
	{
		// With deferUntilVisible the chrome is not built while the view is hidden.
		compare(scHidden.children.length, 0)

		// An explicit build request (as ComputeColumnWindow sends when it becomes
		// visible) builds the chrome, idempotently.
		scHidden.requestBuild()
		verify(scHidden.children.length > 0)

		scHidden.requestBuild()
		verify(scHidden.children.length > 0)

		compare(scHidden.rCode, "")
	}

	function test_load_json_generates_r()
	{
		sc.mode = ScriptConstructorMode.Filter
		sc.constructorJson = '{"formulas":[{"nodeType":"Operator","operator":">","leftArgument":{"nodeType":"Column","columnName":"TestInts","columnTypeUser":-1,"columnTypeDrop":-1},"rightArgument":{"nodeType":"Number","value":2}}]}'

		compare(sc.rCode, "(TestInts.scale > 2)\n")
		compare(sc.somethingChanged, false)
		compare(sc.jsonChanged(), false)
		compare(sc.checkAndApply(), true)
		compare(sc.lastCheckPassed, true)
	}

	function test_check_and_apply_emits()
	{
		sc.mode = ScriptConstructorMode.Filter
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
		sc.mode = ScriptConstructorMode.Filter
		sc.constructorJson = '{"formulas":[{"nodeType":"Operator","operator":"+","leftArgument":{"nodeType":"Column","columnName":"TestInts","columnTypeUser":-1,"columnTypeDrop":-1},"rightArgument":null}]}'

		compare(sc.checkAndApply(), false)
		compare(sc.lastCheckPassed, false)
	}

	function test_non_boolean_root_fails_filter_check()
	{
		sc.mode = ScriptConstructorMode.Filter
		sc.constructorJson = '{"formulas":[{"nodeType":"Operator","operator":"+","leftArgument":{"nodeType":"Number","value":1},"rightArgument":{"nodeType":"Number","value":2}}]}'

		compare(sc.checkAndApply(), false)
	}

	function test_empty_filter_applies()
	{
		sc.mode = ScriptConstructorMode.Filter
		sc.constructorJson = '{"formulas":[]}'
		compare(sc.checkAndApply(), true)
		compare(sc.rCode, "")
	}

	function test_filter_allows_two_formulas()
	{
		sc.mode = ScriptConstructorMode.Filter
		sc.constructorJson = twoBooleanFormulas

		// Filter mode joins any number of formulas with `&`, so two boolean formulas pass.
		compare(sc.checkAndApply(), true)
		compare(sc.lastCheckPassed, true)
	}

	function test_computed_column_rejects_two_formulas()
	{
		sc.mode = ScriptConstructorMode.ComputedColumn
		sc.constructorJson = twoBooleanFormulas

		// A computed column is a single expression: more than one formula fails the check
		// (and the "Only one formula per computed column allowed." hint is column-only).
		compare(sc.checkAndApply(), false)
		compare(sc.lastCheckPassed, false)
	}

	function test_computed_column_empty_applies()
	{
		sc.mode = ScriptConstructorMode.ComputedColumn
		sc.constructorJson = '{"formulas":[]}'

		var appliedR = null
		var handler = function(json, rCode) { appliedR = rCode; }
		sc.applyRequested.connect(handler)

		// An empty computed column passes (applies empty code) and emits an empty rCode.
		compare(sc.checkAndApply(), true)
		compare(sc.lastCheckPassed, true)
		compare(sc.rCode, "")
		compare(appliedR, "")

		sc.applyRequested.disconnect(handler)
	}

	// Regression guard: C++-constructed leaves must keep the view's QQmlContext, otherwise the
	// QQuickImage loader returns an empty pixmap (status Ready but paintedWidth 0) and the
	// watermark/never-sized decoration stays 0x0. Also guards buildOperatorBar() against
	// accumulating duplicate prototypes across mode changes / repeated builds.
	function test_images_paint_and_bar_not_duplicated()
	{
		sc.mode = ScriptConstructorMode.Filter
		sc.constructorJson = twoBooleanFormulas
		verify(sc.checkAndApply())
		sc.visible = true

		var images = []
		function collect(item)
		{
			try { if(item.source !== undefined) images.push(item) } catch(e) {}
			for(var i = 0; i < item.children.length; i++)
				collect(item.children[i])
		}
		collect(sc)
		verify(images.length > 0)
		for(var i = 0; i < images.length; i++)
			compare(images[i].paintedWidth > 0, true, "image paints: " + images[i].source)

		var watermark = null
		for(i = 0; i < images.length; i++)
			if(images[i].z === -2)
				watermark = images[i]
		verify(watermark !== null)
		verify(watermark.width > 0 && watermark.height > 0)

		function countBarPrototypes()
		{
			var count = -1
			function walk(item)
			{
				if(count === -1 && item.z === 3 && item.children.length > 0)
					count = item.children[0].children.length
				for(var i = 0; i < item.children.length; i++)
					walk(item.children[i])
			}
			walk(sc)
			return count
		}
		var prototypes = countBarPrototypes()
		verify(prototypes > 0)
		sc.mode = ScriptConstructorMode.ComputedColumn
		wait(100)
		sc.mode = ScriptConstructorMode.Filter
		wait(100)
		compare(countBarPrototypes(), prototypes)
	}
}
