<a href="jasp-controls-qmlmodule.md" translate="no">JASP.Controls</a>

FactorsList

<div class="sidebar">

<div class="toc">

### Contents

- [Detailed Description](#details)
- [R Binding](#r-binding)
- [Properties](#properties)
- [Signals](#signals)
- [Inherited Properties from
  AssignedVariablesList](#inherited-properties-from-assignedvariableslist)
- [Other Inherited Properties](#other-inherited-properties)
- [Example](#example)

</div>

<div id="sidebar-content" class="sidebar-content">

</div>

</div>

# FactorsList QML Type

An assigned variables list with an editable title, used for individual
factor panels. [More...](#details)

<div class="table">

|                   |                                                        |
|-------------------|--------------------------------------------------------|
| Import Statement: | `import JASP.Controls 1.0`                             |
| Inherits:         | <a href="qml-jasp-controls-assignedvariableslist.md" 
                     translate="no">AssignedVariablesList</a>                |

</div>

- [List of all members, including inherited
  members](qml-jasp-controls-factorslist-members.md)

## Detailed Description

Extends <a href="qml-jasp-controls-assignedvariableslist.md"
translate="no">AssignedVariablesList</a> by adding an editable
<a href="qml-jasp-controls-textfield.md" translate="no">TextField</a>
for the factor title. Typically used inside
<a href="qml-jasp-controls-factorsform.md"
translate="no">FactorsForm</a>; each FactorsList panel represents one
factor and its assigned variables.

## R Binding

- **R Type:** array of variable names
- **Default:** \[\] (empty)

## Properties

- **editableTitle** (string) - The editable factor title text. Default:
  "".

## Signals

- **titleIsChanged()** - Emitted when the factor title is changed by the
  user.

## Inherited Properties from AssignedVariablesList

- **name** (string) - R option name this control binds to. Default: "".
- **dropKeys** (var) - Keys for accepting drag-and-drop. Default: \[\].
- **allowedColumns** (var) - Column types allowed. Default: \[\].
- **allowTypeChange** (bool) - Allow changing variable type icons.
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
FactorsList {
    name: "factor1"
    editableTitle: "Factor 1"
    dropKeys: "allAvailableVariables"
}
```
