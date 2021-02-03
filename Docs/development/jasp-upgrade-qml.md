Guide to setting up an upgrade file for a JASP Module
=====================================================

After you have [made a module](jasp-adding-module.md) and released it you might want to change some things.
The name of an option in your qml form for instance or the name of an analysis.  
To make this possible you can add a file to your module, `Upgrades.qml`, which you place under `ModuleName/inst/` next to your `Description.qml`.
Then whenever a jasp-file is loaded

# Upgrades.qml
This file contains a description of what upgrades your module offers, of which the simplest case is simply [renaming an analysis function](#rename-analysis). Further possibilities are [renaming an option](#rename-option) or [adding an option with a default value](#set-option). You can also [remove an option](#set-option) or even run custom [javascript](#javascript). And in all cases this can be done under certain [conditions](#conditionals) but that will probably be rarely used.

The rough structure of `Upgrades.qml` is an `Upgrades{}` QML Item with one or more `Upgrade{}` items in it. Each of these upgrades consists at least of a `functionName`, `fromVersion` and a `toVersion` field. The `functionName`-field specifies which analysis (called `function` here) is targeted while `fromVersion` and `toVersion` predictably define from which module-version it will be upgraded and to which one. Just having that would do very little and the simplest possible change you might want to make is by adding `newFunctionName` with the new name of the analysis.

A very short example of that would be fixing the typo that someone could have made in 0.9 of a fictional module:
```qml
import QtQuick		2.12
import JASP.Module	1.0

Upgrades
{
	Upgrade
	{
		functionName: 		"Descrpitives"
		newFunctionName:	"Descriptives"
		fromVersion:		"0.9"
		toVersion:			"1.0"
	}
}
```

To actually change the options for the qml form belonging to the analysis/function in question one needs to add one or more `Change*` QML items as described in the rest of this document.

# Changing Options
Besides changing the name of your module or the function called for analysis you can also change the options stored in a jasp-file by older versions of your module. In case you aren't sure what is meant by options: these are the values stored in a json format by the QML form of your analysis. Each option has a name and this is the exact same `name:` as you specify in the QML forms. 

This also means that if you change the name of some option and the user loads a jasp-file made with an older version of your module it will not have a stored value for that particular option. And thus it takes whatever default value is defined in QML. 

The problem here is that the results shown in JASP are also loaded from the jasp-file and are of course based on whatever the options were when the user saved her or his file, and presumably the desired output as well. To make sure that loading a file with a newer version of your module will still lead to the same results as before you can add `Change*`s as specified below.

Something important to keep in mind is that a list of these `Changes` will be applied in the order you've specified them. 

For all following examples the full QML file would look something like:
```qml
import QtQuick		2.12
import JASP.Module	1.0

Upgrades
{
	Upgrade
	{
		functionName:	"SomeAnalysis"
		fromVersion:	"0.1"
		toVersion:		"0.2"
 
		//	Here there would be Change*{} Items, such as:
		ChangeRename { from: "vars"; to: "variables" }
	}
}
```
To keep the examples nice and short the `import` statements and the surrounding `Upgrades{...}` item will be left out from here on out.

## Rename Option
The simplest case is if you renamed an option and you would like that whatever value was saved is also used for the now differently named option on loading with the newer version. For this you add a `ChangeRename` item that specifies what should be renamed to what, which would look something like:
```qml
Upgrade
{
	functionName: "SomeAnalysis";	fromVersion: "0.1";		toVersion: "0.2";

	ChangeRename
	{
		from:	"vars"
		to:		"variables"
	}
}
```
In the above example the option "vars" in version 0.1 of SomeAnalysis is renamed to "variables" in version 0.2 of SomeAnalysis. You could add more renames or other changes to `Upgrade` as needed.

## Copy Option
This is exactly the same as [rename option](#rename-option) except that you use a `ChangeCopy` item and it well, makes a copy instead of renaming. It would look like this:
```qml
Upgrade
{
	functionName: "SomeAnalysis";	fromVersion: "0.1";		toVersion: "0.2";

	ChangeCopy
	{
		from:	"vars"
		to:		"variables"
	}
}
```

## Set Option
Sometimes you might want to set an option to something else and with the `ChangeSetValue` you can give a value that should be set. This can be a javascript object, array, string, bool or number. Which one you should set depends on the kind of qml-item that should handle the value. Several examples follow:

```qml
Upgrade
{
	functionName: "SomeAnalysis";	fromVersion: "0.1";		toVersion: "0.2";

	ChangeSetValue	{ name: "variables";	jsonValue:	[ "contNormal", "contExpon" ];	} //Here we set an array as the new value for variables.
	ChangeSetValue	{ name: "textOpt";		jsonValue:	"Hello World";					} //A string
	ChangeSetValue	{ name: "maxVal";		jsonValue:	0.1;							} //A number
	ChangeSetValue	{ name: "showPlot";		jsonValue:	false;							} //A boolean
	ChangeSetValue	
	{ 
		name:		"complicatedOption";	
		jsonValue:							//We can also give a whole object here and this can be quite useful if the qml-item needs something more complicated
		{
			someProperty:	"a string"
			aProperty:		1.2
		}
	}
}
```
Of course, the array of values could contain objects or the other values, basically as varied as the [JSON standard](https://www.json.org/json-en.html) allows.
Setting options like this on it's own is likely of limited use but if combined with [conditionals](#conditionals) they have quite the potential.


## Javascript
There are cases where the above simpler `Change*` items aren't flexible enough, even painfully using [conditionals](#conditionals), where the solution would be to run a function to determine the desired new value for an option. This could be merging two arrays of values into one or transforming the value of a `RadioButtons{...}` into booleans for `CheckBox{...}`'s. 

For this you can use the `ChangeJS` item. In it you specify the `name` of the option where the result of `jsFunction` should be written. 
While `jsFunction` gets, as its single argument, the entire `options` list as it is at *that moment* (because previous changes are of course already applied).
This way you can be a bit more flexible than in `ChangeSetValue` for instance. But you have to be careful with your code as you would not want to have your upgradecode crashing on somebodies computer and messing up there research documents... Or making it impossible for them to upgrade it beyond that point at least.

Here follows an example where a radiobutton with three values is converted into two checkboxes:
```qml
Upgrade
{
	functionName: "SomeAnalysis";	fromVersion: "0.1";		toVersion: "0.2";

	ChangeJS
	{
		name:			"checkboxDoSomething";
		jsFunction:		function(options) { return options["radiobuttons"] === "something";	}
	}

	ChangeJS
	{
		name:			"checkboxDoSomethingElse";
		jsFunction:		function(options) { return options["radiobuttons"] === "somethingElse";	}
	}
}
```

Many more examples could be given here but the basic takeaway is that `jsFunction` gets an `options` javascript-object consisting of named entries.
Each name is the same `name:` as each item in the qml form for your analysis has and the value is whatever kind of value such an item stores.
An example of the options from a very simple form with a `Checkbox` and a `TextField` would be:
```json
{
	"checkboxA":	true,
	"textEntry":	"some nice string"
}
```
You can access the information in it in any way you like and you can also change it completely. But those changes won't go back to JASP.
Only the returnvalue of `jsFunction` is used and it is *always* stored in the options under the name you gave in `ChangeJS { name: ... }`.

## Remove option
For completion's sake a `ChangeRemove` item was added and it allows for an option to be completely removed from an option list if it isn't necessary anymore.
But honestly, this is not very useful because it would be silently dropped by JASP anyway. However, you can use it like:
```qml
Upgrade
{
	functionName: "SomeAnalysis";	fromVersion: "0.1";		toVersion: "0.2";

	ChangeRemove
	{
		name:	"uselessOption"
	}
}
```

## Conditionals
There are scenarios where you might only want to apply a certain `Change*` when certain other values are set to something in particular.
You could do this through `ChangeJS` item, but that would mean you would have to copy the code for each new value or some other complicated way of fixing that.

Instead you could add a `condition:` entry to any of your `Change*` items specifying whether you would want the change to be applied or not.
The default value for it is `null` and in that case it is interpreted as `true`, just as when you set it to `true` manually.

It could also have function like the `jsFunction` from [ChangeJS](#javascript) that returns a boolean and can check the whole options list. This can be used in many ways but a rather random example will follow just to get a feel of the possibilities. It shows how you can reuse the same `condition` function multiple times, but keep in mind that it will need to run every time. So if `bogusText` changes between `weirdRename` and and anything that uses the value of `weirdRename.condition` it won't return the same value and those changes might not be applied.
```qml
Upgrade
{
	functionName: "SomeAnalysis";	fromVersion: "0.1";		toVersion: "0.2";

	ChangeRename
	{
		id:			weirdRename

		//Only rename egg to easter if an option called "bogusText" equals "easterEgg":
		condition:	function(options) { return options["bogusText"] === "easterEgg";  }
		from:		"egg"
		to:			"easter"
	}

	ChangeCopy
	{
		//reuse the condition from before to copy the value from easter back to egg.
		condition:	weirdRename.condition
		from:		"easter"
		to:			"egg"
	}

	ChangeRemove
	{
		//And remove easter again to make this all entirely useless
		condition:	function(options) { return weirdRename.condition(options); }
		name:		"easter"
	}
}
```

You could also set it like:
```qml
Upgrade
{
	functionName: "SomeAnalysis";	fromVersion: "0.1";		toVersion: "0.2";

	ChangeRename
	{
		id:			weirdRename
		from:		"egg";		
		to:			"easter";
		condition:	false //It will never be applied. But if you change this value programmatically the value will be kept as long as you module & JASP run.
		//This means that after it is changed later any upgrades will then have this Change applied!
	}

	ChangeRemove
	{
		id: 		dangerousChange
		name:		itDoesntReallyMatter
		condition:	function(options)
		{
			weirdRename.condition = true; 
			// So loading the first SomeAnalysis being upgraded this run did not have the weird rename.
			// But because this condition is checked *afterwards* the next SomeAnalysis-upgrade will have the rename...
			// Because we've just set it to "true" and the value is kept.
			// It is very unlikely this is ever desirable but I'm showing it here to make modulewriters aware of the pitfalls of hacky code.
			return false;
		}
	}
}
```

And now for a useful example of something someone might actually want to do and a transposition of an actual upgrade done archaically in JASP:
```qml
Upgrade
{
  // For version 0.12 we changed a lot of analyses and this also simplified some forms a bit.
  functionName:	"Anova"
  fromVersion:	"0.11.1"
  toVersion:	"0.12"

	// If homogeneityTests was set false then that meant that some other options were ignored, from 0.12 this option was removed and those three checkboxes are used directly.

	ChangeSetValue
	{
		id:			homogeneityCorrections
		condition:	function(options) { return !options["homogeneityCorrections"]; }
		name:		"homogeneityNone"
		jsonValue:	true
	}

	ChangeSetValue
	{
		condition:	homogeneityCorrections.condition
		name:		"homogeneityBrown"
		jsonValue:	false
	}

	ChangeSetValue
	{
		condition:	homogeneityCorrections.condition
		name:		"homogeneityWelch"
		jsonValue:	false
	}

}
```
