[JASP.Controls](jasp-controls-qmlmodule.md)

AssignedVariablesList


# AssignedVariablesList QML Type

A variable list preset for assigned variable selection.


|                   |                                                |
|-------------------|------------------------------------------------|
| Import Statement: | `import JASP.Controls 1.0`                     |
| Inherits:         | [VariablesList](qml-jasp-controls-variableslist.md)                |
| Inherited By:     | [FactorsList](qml-jasp-controls-factorslist.md)                  |


- [List of all members, including inherited
  members](qml-jasp-controls-assignedvariableslist-members.md)

## Detailed Description

Extends [VariablesList](qml-jasp-controls-variableslist.md) with drop-insert mode for standard
variable assignment. This is the default target list used in a
[VariablesForm](qml-jasp-controls-variablesform.md).

## R Binding

- **R Type:** list or character vector
- **Default:** \[\] (empty array)

## Inherited Properties from VariablesList

- **name** (string) - R option name this control binds to. Default: "".
- **title** (string) - Title displayed above the list. Alias: label.
  Default: "".
- **singleVariable** (bool) - Limit to one variable (sets maxRows: 1).
  Default: false.
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
AssignedVariablesList {
    name: "dependent"
    title: qsTr("Dependent Variable")
    singleVariable: true
    allowedColumns: ["scale"]
}
```
