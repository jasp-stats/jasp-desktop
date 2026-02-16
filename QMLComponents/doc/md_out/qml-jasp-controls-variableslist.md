<a href="jasp-controls-qmlmodule.md" translate="no">JASP.Controls</a>

VariablesList

<div class="sidebar">

<div class="toc">

### Contents

- [Detailed Description](#details)
- [R Binding](#r-binding)
- [Properties](#properties)
- [Inherited Properties](#inherited-properties)
- [Signals](#signals)
- [Example](#example)

</div>

<div id="sidebar-content" class="sidebar-content">

</div>

</div>

# VariablesList QML Type

The primary variable selection control in JASP. [More...](#details)

<div class="table">

|  |  |
|----|----|
| Import Statement: | `import JASP.Controls 1.0` |
| Inherited By: | <a href="qml-jasp-controls-assignedpairsvariableslist.md"
translate="no">AssignedPairsVariablesList</a>, <a href="qml-jasp-controls-assignedrepeatedmeasurescells.md"
translate="no">AssignedRepeatedMeasuresCells</a>, <a href="qml-jasp-controls-assignedvariableslist.md"
translate="no">AssignedVariablesList</a>, <a href="qml-jasp-controls-availablevariableslist.md"
translate="no">AvailableVariablesList</a>, and <a href="qml-jasp-controls-modeltermslist.md"
translate="no">ModelTermsList</a> |

</div>

- [List of all members, including inherited
  members](qml-jasp-controls-variableslist-members.md)

## Detailed Description

Displays a list where users can drag-and-drop variables from the
dataset. This is usually used inside a
<a href="qml-jasp-controls-variablesform.md"
translate="no">VariablesForm</a>

## R Binding

- **R Type:** list or character vector
- **Default:** \[\] (empty array)

## Properties

- **name** (string) - R option name this control binds to. Default: "".
- **title** (string) - Title displayed above the list. Alias: label.
  Default: "".
- **singleVariable** (bool) - Limit to one variable (sets maxRows: 1).
  Default: false.
- **maxRows** (int) - Maximum variables allowed (-1 = unlimited).
  Default: -1.
- **listViewType** (enum) - Type: JASP.AssignedVariables,
  JASP.Interaction, JASP.RepeatedMeasures, JASP.Layers.
- **allowedColumns** (array) - Restrict to column types: "scale",
  "ordinal", "nominal". Default: \[\].
- **draggable** (bool) - Allow drag-and-drop operations. Default: true.
- **showVariableTypeIcon** (bool) - Display variable type icons.
  Default: true.
- **source** (var) - Source for populating the VariablesList. Per
  default it will be all variables. Can be set to an id or a name (or a
  combination) of other controls having variables.
- **rowComponent** (Component) - QML component for custom row controls.
  Can be used to add e.g. a
  <a href="qml-jasp-controls-checkbox.md" translate="no">CheckBox</a>
  for each variable in the VariablesList.

## Inherited Properties

- **enabled** (bool) - Whether the control is interactive. Default:
  true.
- **visible** (bool) - Whether the control is visible. Default: true.
- **info** (string) - Info that will be used by tooltip and to generate
  the help. Default: "".
- **toolTip** (string) - This property overwrite info property, in order
  to display a simpler tooltip text. Default: "".

## Signals

- **itemDoubleClicked(int index)** - User double-clicked a variable.
- **itemsDropped(indexes, dropList, dropItemIndex)** - Variables were
  dropped.
- **selectedItemsChanged()** - Selection changed.

## Example

``` qml
Column {
    VariablesList {
        name: "dependent"
        title: qsTr("Dependent Variable")
        singleVariable: true
        allowedColumns: ["scale"]
    }

    VariablesList {
        name: "modelTerms"
        title: qsTr("Model Terms")
        listViewType: JASP.Interaction
        rowComponent: CheckBox { name: "isNuisance" }
    }
}
```
