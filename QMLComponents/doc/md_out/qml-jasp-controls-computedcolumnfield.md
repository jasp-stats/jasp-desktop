<a href="jasp-controls-qmlmodule.md" translate="no">JASP.Controls</a>

ComputedColumnField

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

# ComputedColumnField QML Type

A text field for entering a computed column name. [More...](#details)

<div class="table">

|  |  |
|----|----|
| Import Statement: | `import JASP.Controls 1.0` |
| Inherits: | <a href="qml-jasp-controls-textfield.md" translate="no">TextField</a> |

</div>

- [List of all members, including inherited
  members](qml-jasp-controls-computedcolumnfield-members.md)

## Detailed Description

Extends
<a href="qml-jasp-controls-textfield.md" translate="no">TextField</a>
with the "computedColumn" input type. The entered name is used to create
or reference a computed column in the dataset. Validation ensures the
column name is valid and available.

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
ComputedColumnField {
    name: "generatedColumn"
    label: qsTr("Column name")
}
```
