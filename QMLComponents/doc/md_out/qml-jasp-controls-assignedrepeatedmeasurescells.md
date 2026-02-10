<a href="jasp-controls-qmlmodule.md" translate="no">JASP.Controls</a>

AssignedRepeatedMeasuresCells

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

# AssignedRepeatedMeasuresCells QML Type

A variable list preset for assigning variables to repeated measures
cells. [More...](#details)

<div class="table">

|                   |                                                |
|-------------------|------------------------------------------------|
| Import Statement: | `import JASP.Controls 1.0`                     |
| Inherits:         | <a href="qml-jasp-controls-variableslist.md" 
                     translate="no">VariablesList</a>                |

</div>

- [List of all members, including inherited
  members](qml-jasp-controls-assignedrepeatedmeasurescells-members.md)

## Detailed Description

Extends <a href="qml-jasp-controls-variableslist.md"
translate="no">VariablesList</a> with a two-column layout designed for
repeated measures designs. Uses drop-replace mode and restricts to scale
variables only. Typically used within a
<a href="qml-jasp-controls-variablesform.md"
translate="no">VariablesForm</a> alongside a
<a href="qml-jasp-controls-factorsform.md"
translate="no">FactorsForm</a> that defines the repeated measures
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
