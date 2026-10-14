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

	function findItems(item, property, result)
	{
		// Find recursively all the items having this property
		var children = []
		for (var i = 0; i < item.children.length; i++)
			children.push(item.children[i])

		// The buttons of a TabBar (or any Container) are in its contentModel, not in its children
		if (item.contentModel !== undefined && typeof item.itemAt === "function")
			for (var j = 0; j < item.count; j++)
				children.push(item.itemAt(j))

		for (var child of children)
		{
			if (!child || result.indexOf(child) >= 0)
				continue
			if (child.hasOwnProperty(property))
				result.push(child)
			findItems(child, property, result)
		}
		return result
	}

	function lockState(list)
	{
		// The terms of a list, with '(locked)' when a term cannot be selected (and so cannot be moved)
		var result = []
		var names = toArray(list.columnsNames)
		for (var i = 0; i < names.length; i++)
		{
			list.model.clearSelectedItems()
			list.model.selectItem(i, true)
			result.push(names[i] + (list.model.selectedItems().length > 0 ? "" : "(locked)"))
		}
		list.model.clearSelectedItems()
		return result
	}

	function checkLockState(list, expected, message)
	{
		tryVerify(function() { return JSON.stringify(lockState(list)) === JSON.stringify(expected) }, 1000,
				  message + ": expected " + JSON.stringify(expected) + " but got " + JSON.stringify(lockState(list)))
	}

	function moveTerms(from, to, names)
	{
		// Move without checking the result: in a nested FactorsForm, a target list may already have these terms
		from.clearSelectedItems()
		var all = toArray(from.columnsNames)
		for (var name of names)
		{
			var index = all.indexOf(name)
			verify(index >= 0, "Variable " + name + " not found in " + from.name)
			from.addSelectedItem(index)
		}
		compare(from.model.selectedItems().length, names.length, "Variables " + JSON.stringify(names) + " of " + from.name + " should be movable")
		from.moveSelectedItems(to)
	}
}
