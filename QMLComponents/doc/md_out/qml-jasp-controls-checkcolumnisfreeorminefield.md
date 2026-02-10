<a href="jasp-controls-qmlmodule.md" translate="no">JASP.Controls</a>

CheckColumnIsFreeOrMineField

<div class="sidebar">

<div class="toc">

### Contents

- [Detailed Description](#details)
- [R Binding](#r-binding)
- [Inherited Properties from
  TextField](#inherited-properties-from-textfield)
- [Other Inherited Properties](#other-inherited-properties)
- [Example](#example)

</div>

<div id="sidebar-content" class="sidebar-content">

</div>

</div>

# CheckColumnIsFreeOrMineField QML Type

A text field that validates whether a column name is free or owned by
the current analysis. [More...](#details)

<div class="table">

|  |  |
|----|----|
| Import Statement: | `import JASP.Controls 1.0` |
| Inherits: | <a href="qml-jasp-controls-textfield.md" translate="no">TextField</a> |

</div>

- [List of all members, including inherited
  members](qml-jasp-controls-checkcolumnisfreeorminefield-members.md)

## Detailed Description

Extends
<a href="qml-jasp-controls-textfield.md" translate="no">TextField</a>
with the "checkColumn" input type, which ensures the entered column name
is either not yet used in the dataset or already belongs to the current
analysis. Typically used when an analysis creates or claims a computed
column.

## R Binding

- **R Type:** `character`
- **Default:** ""

## Inherited Properties from TextField

- **name** (string) - R option name this control binds to. Default: "".
- **value** (string) - Current text value. Default: "".
- **label** (string) - Label displayed before the field. Default: "".
- **afterLabel** (string) - Label displayed after the field. Default:
  "".
- **fieldWidth** (int) - Width of the input field. Default: 200.
- **placeholderText** (string) - Greyed text shown when field is empty.
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
CheckColumnIsFreeOrMineField {
    name: "computedColumn"
    label: qsTr("Column name")
}
```
