Guide to setting up an upgrade file for a JASP Module
=====================================================

After you have [made a module](jasp-adding-module.md) and released it you might want to change some things.
The name of an option in your qml form for instance or the name of an analysis.  
To make this possible you can add a file to your module, `upgrades.json`, which you place under `ModuleName/inst/` next to your `description.json`.
Then whenever a jasp-file is loaded

# upgrades.json
This file contains a description of what upgrades your module offers, the simplest case is that of [renaming an analysis function](#rename-analysis) or of moving an analysis from [another module to yours](#move-from-module). Further possibilities are [renaming an option](#rename-option) or [adding an option with a default value](#set-option). You can also [remove an option](#set-option). And in all cases this can be done under certain [conditions](#conditionals) but that will be rare.

The rough structure of `upgrades.json` is an array of analysis & version specific upgrades. Each of these upgrades consists at least of a `from` and a `to` field. The `from`-field specifies which analysis (called `function` here) and version will be upgraded and possibly from which module (if it wasn't in your module yet). The `to`-field then specifies what version and name the analysis will have after the upgrade, usually you will just want to change the version. 

Besides those two fields you can, but need not, add `options` which enables you to change some of the options in the qml form, see the relevant sections under [changing options](#changing-options). And furthermore you can add comments to it simply by adding a `comment` field or something else, it will be ignored by JASP but might be quite useful to you or anyone else reading the upgrades later.

You might want to give the user feedback sometimes, this can be done for the entire analysis through `msg` which can be placed next to `from`, `to` and `options`. This can contain a string and it, together with other messages that might have been added during other upgrade-steps, will be shown above the analysis form.

## Rename Analysis
Suppose you have an analysis function called "BuaetifulAnalis" in the previous version (0.0.1) of your module and you realize you made a *small* spelling mistake and you would like to correct it, but you also want any users of your module to still be able to load their jasp-files using this analysis. 
It can be done as follows:
```json
{
  "from": { "version": "0.0.1", "function": "BuaetifulAnalis"   },
  "to":   { "version": "0.1.0", "function": "BeautifulAnalysis" }
}
```
Keep in mind that the `function` is quite literally the function in R that actually embodies your analysis, if you are fine with that functionname but simply want to change the title it is given in the ribbon-menu or as title then you can do this in `description.json` by changing the appropriate `title` field.

## Move from Module
This scenario is most likely only interesting for core JASP developers that want to move an analysis to another module. But it is mentioned here for good measure. Suppose you have an analysis called "SomeAnalysis" in a module called "aModule" and you would like to move it into your module, this can be done as follows:
```json
{
    "from": { "module": "aModule", "version": "0.1.1", "function": "SomeAnalysis" },
    "to":   {                      "version": "0.2"    }
}
```
This can in fact be combined with renaming the analysis, simply add `function` to the `to` field and give it another name. As you can see you do not need to add `module` to the `to` field, because you can only move it to your own module anyway.

# Changing Options
Besides changing the name of your module or the function called for analysis you can also change the options stored in a jasp-file for older versions of your module. In case you aren't sure what is meant by options: these are the values stored in a json format by the QML form of your analysis. Each option has a name and this is the exact same "name" as you specify in the QML forms. 

This also means that if you change the name of some option and the user loads a jasp-file made with an older version of your module it will not have a stored value for that particular option. And thus it takes whatever default value is defined in QML. The problem here is that the results that are shown in JASP are also loaded from the jasp-file and are of course based on whatever the options were when the user saved her or his file. To make sure that loading a file with a newer version of your module will still lead to the same results as before you can use the configurations specified below.

In most of these cases the `from` and `to` are quite simple and something like:
```json
{
    "from":         { "version": "0.1",     "function": "SomeAnalysis"    },
    "to":           { "version": "0.2"                                    },
    "options":      []
}
```
Module doesn't need to be specified because the default is assuming it is from yours.

Under the `options`-field then you will define an array of "change-objects", that tell JASP what should be changed and optionally when. 
These objects can also contain `msgs` that allow you to add a warning to an option so that you may inform the user of any breaking changes. They will be made visible in the form.
An example of one of these change-objects would be:
```json
{
    "comment":       "This optional field is a comment and completely ignored by JASP. But we humans can read it of course!",
    "conditional":     
    { 
        "_boolOp":   "NOT",
        "_args":     [{"option": false}]
    },
    "changes":       { "optionNew": "This value will only be set in optionNew if the above conditional is true and that means that \"option\" must be true."},
    "msgs":            
    { 
        "optionNew": "This message will be shown as a warning on the just set/added option \"optionNew\".",
        "option":    "While this would be above \"option\" and mention that the whole field \"msgs\" is entirely optional!"
    }
}
```
Something important to keep in mind is that a list of `changes` will be applied in the order you specify them. 
But within one `changes` block you can have [renames](#rename-option) or [setting values](#set-value) and **_this order is ignored!_**.
So if you want to make sure that something is done in order _place them in their own `changes` block!_

## Rename Option
The simplest case is if you renamed an option and you would like that whatever value was saved is also used for the now differently named option. 
To do this you specify the appropriate `from` and `to` so that JASP knows which analysis should be upgraded and you add an `options`-field. 

What you then get would look something like:
```json
{
    "from":     { "version": "0.1",     "function": "SomeAnalysis"    },
    "to":       { "version": "0.2"                                    },
    "options":  [
        { 
            "changes": 
            { 
                "variables": { "_renamed": "vars" } 
            }
        }
    ]
}
```
In the above example the option "vars" in version 0.1 of SomeAnalysis is renamed to "variables" in version 0.2 of SomeAnalysis. You could add more renames to the `changes` object if you need more.

## Copy Option
This is almost exactly the same as [rename option](#rename-option) except that you enter `_copied` instead of `_renamed` and it leaves the old option in place. It would look like this:
```json
{
    "from":     { "version": "0.1",     "function": "SomeAnalysis"    },
    "to":       { "version": "0.2"                                    },
    "options":  [ 
        { 
            "changes": 
            { 
                "variables": { "_copied": "vars" } 
            }
        }
    ]
}
```


## Set Option
Sometimes you might want to set an option to something and in that case the `changes` object would look something like:
```json
{
    "changes":
    {
        "optionA": true,
        "optionB": 3.14,
        "optionC": null
    }
}
```
Here, regardless of if the options already exist and are set to something or not the options `optionA` & `optionB` are added and initialized to true and 3.14 respectively. This can be combined with a rename as well if you like. `optionC` is set to "null" and that means it will be erased from the options.

Setting options like this on it's own is likely of limited use but if combined with conditionals they can unleash quite some potential.

### Setting a boolean
You can also set a value to `true` or `false` based on the current contents of the options, just place a [conditional](#conditionals) in it like so:
```json
{
    "changes":
    {
        "optionAOrB": {
            "_boolOp":   "OR",
            "_args":     [ { "stringOpt": "A" }, { "stringOpt": "B" } ]
        }
    }
}
```

## Conditionals
If you read the format description for renaming or setting options you might have wondered why there was a need to have a `changes` field within the object in `options`. Would it not have been easier to have it one level higher? And you would be right for thinking that. 

However, conditionals are the reason for it and to give some body to that statement, regard the following json:
```json
[
    { 
        "conditional": 
        {
            "_boolOp":   "OR",
            "_args":     [ { "stringOpt": "A" }, { "stringOpt": "B" } ]
        },

        "changes": 
        { 
            "stringOpt": null,
            "boolAB":    true
        }
    },
    {
        "conditional": 
        {
            "_boolOp":   "NOT",
            "_args":     [ 
                { 
                    "_boolOp":  "OR", 
                    "_args":    [ { "stringOpt": "A" }, { "stringOpt": "B" } ]    
                }     
            ]
        },
        "changes": { "boolAB": false } 
    }
]
```
Here we see the added field `conditional` which allows us to have something like an if-statement in the upgrade. This is a bit unwieldy but it is necessary to allow some more complicated upgrades to occur. The basic structure here is that whenever a `conditional` is evaluated as "true" the `changes` next to it are applied to the saved options. 
In this case that means that if the saved file had `stringOpt` set to "A" or "B" it will set `boolAB` to "true" and removes `stringOpt` from the options.

Of course, in this scenario `stringOpt` is not removed from the options if it is set to anything else then "A" or "B".
Because there is an extra changes object later in the `options`-list that sets `boolAB` to "false" under a more complicated conditional that is simply the negative of the earlier one we at least know that `boolAB` will be set to something.

### Definition of Conditional
A conditional in `upgrades.json` consists of the [boolean](https://en.wikipedia.org/wiki/Boolean_algebra) operation to use and one or more arguments. 
The field `_boolOp` specifies what kind of boolean operation is applied and `_args` defines what arguments are evaluated.

The following values of `_boolOp` are evaluated as
- `AND`: all arguments evaluate to "true"
- `OR`: one or more arguments evaluate to "true"
- `XOR`: a single, and no more, argument evaluates to "true"
- `NOT`: the first argument evaluate to "false"

Then for `_args` we have an array of objects that can contain either a segment of the options or another conditional.

#### Option Segment Argument
In case of a segment of options (like `{ "stringOpt": "A" }` in the above example) it is evaluated as "true" when all options in the segment are present in the options and exactly so as the argument states. So in this case it would require `stringOpt` to be "A" and that is it.
In a case like `{ "stringOpt": "A", "numOpt": 2}` then the argument only evaluates to "true" if `numOpt` is 2 ánd `stringOpt` is "A".

Notice how the boolean operator is not yet mentioned here? That is because it only cares about the evaluation of entire segments. 

#### Conditional Argument
Something one might want to combine some kinds of boolean operations, to allow this one can also specify a conditional as an argument. The example already shows an example of it by nesting "OR" in "NOT".
Nesting like this will usually be overkill but could come in handy in certain scenarios.

#### An actual example
The following upgrade was included in JASP for version 0.12 and shows what practically can be done with a conditional and [setting options directly](#set-option).
```json
{
  "comment": "For version 0.12 we changed a lot of analyses and this also simplified some forms a bit.",
  "from":    { "module": "ANOVA", "version": "0.11.1", "function": "Anova" },
  "to":      { "module": "ANOVA", "version": "0.12"                        },
  "options":
  [
    {
        "comment":      "If homogeneityTests was set false then that meant that some other options were ignored, from 0.12 this option was removed and those three checkboxes are used directly.",
        "conditional":  { "_args": [ { "homogeneityCorrections": false }] },
        "changes":
        { 
            "homogeneityNone":    true,
            "homogeneityBrown":   false,
            "homogeneityWelch":   false
        }
    }
  ]
}
```