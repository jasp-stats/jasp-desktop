[JASP.Controls](jasp-controls-qmlmodule.md)

AssignedRepeatedMeasuresCells


# AssignedRepeatedMeasuresCells QML Type

A variable list preset for assigning variables to repeated measures
cells. [More...](#details)


|                   |                                                |
|-------------------|------------------------------------------------|
| Import Statement: | `import JASP.Controls 1.0`                     |
| Inherits:         | [VariablesList](qml-jasp-controls-variableslist.md)                |


- [List of all members, including inherited
  members](qml-jasp-controls-assignedrepeatedmeasurescells-members.md)

## Detailed Description

Extends [VariablesList](qml-jasp-controls-variableslist.md) with a two-column layout designed for
repeated measures designs. Uses drop-replace mode and restricts to scale
variables only. Typically used within a
[VariablesForm](qml-jasp-controls-variablesform.md) alongside a
[FactorsForm](qml-jasp-controls-factorsform.md) that defines the repeated measures
factors.

## R Binding

- **R Type:** list
- **Default:** \[\] (empty array)

## Inherited Properties from VariablesList

- **name** (string) - R option name this control binds to. Default: "".
- **title** (string) - Title displayed above the list. Alias: label.
  Default: "".
- **allowedColumns** (array) - Restrict to column types: "scale",
  "ordinal", "nominal". Default: \[\].
- **showVariableTypeIcon** (bool) - Display variable type icons.
  Default: false.

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
AssignedRepeatedMeasuresCells {
    name: "repeatedMeasuresCells"
    title: qsTr("Repeated Measures Cells")
}
```
