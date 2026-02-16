<a href="jasp-controls-qmlmodule.md" translate="no">JASP.Controls</a>

VariablesForm

<div class="sidebar">

<div class="toc">

### Contents

- [Detailed Description](#details)
- [Properties](#properties)
- [Example](#example)

</div>

<div id="sidebar-content" class="sidebar-content">

</div>

</div>

# VariablesForm QML Type

A two-column layout with available variables on the left and assigned
lists on the right. [More...](#details)

<div class="table">

|                   |                            |
|-------------------|----------------------------|
| Import Statement: | `import JASP.Controls 1.0` |

</div>

- [List of all members, including inherited
  members](qml-jasp-controls-variablesform-members.md)

## Detailed Description

Creates a form where an
<a href="qml-jasp-controls-availablevariableslist.md"
translate="no">AvailableVariablesList</a> and one or more
<a href="qml-jasp-controls-assignedvariableslist.md"
translate="no">AssignedVariablesList</a> are connected. It creates
automatically the Arrow buttons for each
<a href="qml-jasp-controls-assignedvariableslist.md"
translate="no">AssignedVariablesList</a> and sets their height to fill
the form.

<div class="admonition note">

**Note:** VariablesForm does not bind directly to R options. The child
<a href="qml-jasp-controls-variableslist.md"
translate="no">VariablesList</a> controls each have their own R binding.

</div>

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
