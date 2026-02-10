<a href="jasp-controls-qmlmodule.md" translate="no">JASP.Controls</a>

AssignedVariablesList

<div class="sidebar">

<div class="toc">

### Contents

- [Detailed Description](#details)
- [R Binding](#r-binding)
- [Inherited Properties from
  VariablesList](#inherited-properties-from-variableslist)
- [Other Inherited Properties](#other-inherited-properties)
- [Example](#example)

</div>

<div id="sidebar-content" class="sidebar-content">

</div>

</div>

# AssignedVariablesList QML Type

A variable list preset for assigned variable selection.
[More...](#details)

<div class="table">

|                   |                                                |
|-------------------|------------------------------------------------|
| Import Statement: | `import JASP.Controls 1.0`                     |
| Inherits:         | <a href="qml-jasp-controls-variableslist.md" 
                     translate="no">VariablesList</a>                |
| Inherited By:     | <a href="qml-jasp-controls-factorslist.md"   
                     translate="no">FactorsList</a>                  |

</div>

- [List of all members, including inherited
  members](qml-jasp-controls-assignedvariableslist-members.md)

## Detailed Description

Extends <a href="qml-jasp-controls-variableslist.md"
translate="no">VariablesList</a> with drop-insert mode for standard
variable assignment. This is the default target list used in a
<a href="qml-jasp-controls-variablesform.md"
translate="no">VariablesForm</a>.

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
