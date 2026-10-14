import QtTest
import QtQuick

// Base TestCase for the tst_specialSources*.qml files: automated versions of the sections of the jaspTestModule analysis
// "Test Sources with special attributes" (testSpecialSources.qml).
// The quicktest dataset (see Tests/testqml.cpp) has: TestDoubles (scale), TestInts (ordinal), TestLetters (nominal) & TestNominal (nominal)
//
// The "Duplicate the analysis / load a JASP file" checks are mimicked with reloadOptions (form.options() & form.setOptions()).

TestCase
{
	// The derived TestCase must declare 'property alias form: jaspForm':
	// the form must be in the context of the JASPControls as soon as they are created

	SignalSpy
	{
		id:				spyLoader
		target:			form
		signalName:		"formCompletedSignal"
	}

	function initTestCase()
	{
		spyLoader.wait(1000)
		compare(spyLoader.count, 1)
	}

	function toArray(list)
	{
		var result = []
		for (var i = 0; i < list.length; i++)
			result.push(list[i])
		return result
	}

	function sameList(list, expected)
	{
		return JSON.stringify(toArray(list)) === JSON.stringify(expected)
	}

	function checkList(control, expected, message)
	{
		tryVerify(function() { return sameList(control.columnsNames, expected) }, 1000,
				  message + ": expected " + JSON.stringify(expected) + " but got " + JSON.stringify(toArray(control.columnsNames)))
	}

	function moveVariables(from, to, names)
	{
		var fromExpected	= toArray(from.columnsNames).filter(function(name) { return names.indexOf(name) < 0 })
		var toCount			= to.count + names.length

		from.clearSelectedItems()
		for (var i = 0; i < names.length; i++)
		{
			var index = toArray(from.columnsNames).indexOf(names[i])
			verify(index >= 0, "Variable " + names[i] + " not found in " + from.name)
			from.addSelectedItem(index)
		}
		from.moveSelectedItems(to)

		tryVerify(function()
		{
			var toNames = toArray(to.columnsNames)
			return toNames.length === toCount && names.every(function(name) { return toNames.indexOf(name) >= 0 })
		}, 1000, "Variables " + JSON.stringify(names) + " should be moved to " + to.name + ", which has now " + JSON.stringify(toArray(to.columnsNames)))
		checkList(from, fromExpected, "Variables " + JSON.stringify(names) + " should be removed from " + from.name)
	}

	function inter(names)
	{
		return names.join(INTERACTION_SEPARATOR)
	}

	function rowControl(list, key, name)
	{
		var control = list.getRowControl(key, name)
		verify(control, "Row control " + name + " for " + key + " should exist in " + list.name)
		return control
	}

	function reloadOptions(options)
	{
		// This mimics the loading of a JASP file, or the duplication of an analysis, with these options
		form.setOptions(options)
		compare(form.errors, "", "Loading the options should not give any error")
	}
}
