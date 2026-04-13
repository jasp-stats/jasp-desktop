[JASP.Controls](jasp-controls-qmlmodule.md)

AvailableVariablesList


# AvailableVariablesList QML Type

The source list showing all available dataset variables.


|                   |                                                |
|-------------------|------------------------------------------------|
| Import Statement: | `import JASP.Controls 1.0`                     |
| Inherits:         | [VariablesList](qml-jasp-controls-variableslist.md)                |


- [List of all members, including inherited
  members](qml-jasp-controls-availablevariableslist-members.md)

## Detailed Description

Extends [VariablesList](qml-jasp-controls-variableslist.md) configured as the source (available)
list in a [VariablesForm](qml-jasp-controls-variablesform.md). This control is not bound to R
options. It displays all dataset variables from which users can drag
variables into assigned lists.


**Note:** AvailableVariablesList does not bind to R options. It is
automatically managed by [VariablesForm](qml-jasp-controls-variablesform.md).


## Inherited Properties from VariablesList

- **title** (string) - Title displayed above the list. Alias: label.
  Default: "".

## Other Inherited Properties

- **enabled** (bool) - Whether the control is interactive. Default:
  true.
- **visible** (bool) - Whether the control is visible. Default: true.
- **info** (string) - Info that will be used by tooltip and to generate
  the help. Default: "".
- **toolTip** (string) - This property overwrite info property, in order
  to display a simpler tooltip text. Default: "".

## Example

``` qml
VariablesForm {
    AvailableVariablesList {
        name: "allVariables"
    }
    AssignedVariablesList {
        name: "dependent"
        singleVariable: true
    }
}
```
