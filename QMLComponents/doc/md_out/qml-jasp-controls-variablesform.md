[JASP.Controls](jasp-controls-qmlmodule.md)

VariablesForm


# VariablesForm QML Type

A two-column layout with available variables on the left and assigned
lists on the right. [More...](#details)


|                   |                            |
|-------------------|----------------------------|
| Import Statement: | `import JASP.Controls 1.0` |


- [List of all members, including inherited
  members](qml-jasp-controls-variablesform-members.md)

## Detailed Description

Creates a form where an
[AvailableVariablesList](qml-jasp-controls-availablevariableslist.md) and one or more
[AssignedVariablesList](qml-jasp-controls-assignedvariableslist.md) are connected. It creates
automatically the Arrow buttons for each
[AssignedVariablesList](qml-jasp-controls-assignedvariableslist.md) and sets their height to fill
the form.


**Note:** VariablesForm does not bind directly to R options. The child
[VariablesList](qml-jasp-controls-variableslist.md) controls each have their own R binding.


## Properties

- **listWidth** (int) - Width of each variable list. Default: width \* 2
  / 5.
- **removeInvisibles** (bool) - Remove invisible controls from the
  layout. Default: false.

## Example

``` qml
VariablesForm {
    AvailableVariablesList { name: "allVariables" }
    AssignedVariablesList { name: "dependent"; title: qsTr("Dependent Variable"); singleVariable: true }
    AssignedVariablesList { name: "fixedFactors"; title: qsTr("Fixed Factors"); allowedColumns: ["nominal"] }
}
```
