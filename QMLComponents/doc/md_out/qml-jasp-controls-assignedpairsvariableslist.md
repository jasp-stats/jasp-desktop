[JASP.Controls](jasp-controls-qmlmodule.md)

AssignedPairsVariablesList


# AssignedPairsVariablesList QML Type

A variable list preset for paired variable assignment.


|                   |                                                |
|-------------------|------------------------------------------------|
| Import Statement: | `import JASP.Controls 1.0`                     |
| Inherits:         | [VariablesList](qml-jasp-controls-variableslist.md)                |


- [List of all members, including inherited
  members](qml-jasp-controls-assignedpairsvariableslist-members.md)

## Detailed Description

Extends [VariablesList](qml-jasp-controls-variableslist.md) with a two-column layout where
variables are assigned in pairs. Uses drop-replace mode so each slot
accepts exactly one variable.

## R Binding

- **R Type:** list of paired character vectors
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
AssignedPairsVariablesList {
    name: "pairs"
    title: qsTr("Variable Pairs")
}
```
