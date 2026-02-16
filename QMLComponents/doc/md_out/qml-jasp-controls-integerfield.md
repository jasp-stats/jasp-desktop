<a href="jasp-controls-qmlmodule.md" translate="no">JASP.Controls</a>

IntegerField

<div class="sidebar">

<div class="toc">

### Contents

- [Detailed Description](#details)
- [R Binding](#r-binding)
- [Properties](#properties)
- [Inherited Properties from
  TextField](#inherited-properties-from-textfield)
- [Other Inherited Properties](#other-inherited-properties)
- [Example](#example)

</div>

<div id="sidebar-content" class="sidebar-content">

</div>

</div>

# IntegerField QML Type

A text field preset for entering integer values. [More...](#details)

<div class="table">

|  |  |
|----|----|
| Import Statement: | `import JASP.Controls 1.0` |
| Inherits: | <a href="qml-jasp-controls-textfield.md" translate="no">TextField</a> |

</div>

- [List of all members, including inherited
  members](qml-jasp-controls-integerfield-members.md)

## Detailed Description

Extends
<a href="qml-jasp-controls-textfield.md" translate="no">TextField</a>
with the "integer" input type and a JASPDoubleValidator configured for
zero decimal places. Supports configurable minimum, maximum, and sign
constraints.

## R Binding

- **R Type:** `integer`
- **Default:** 0

## Properties

- **negativeValues** (bool) - Allow negative integers. Default: false.
- **min** (int) - Minimum allowed value. Default: 0 (or -2147483647 when
  negativeValues is true).
- **max** (int) - Maximum allowed value. Default: 2147483647.
- **inclusive** (enum) - Whether min/max bounds are inclusive. Default:
  JASP.MinMax. Can have also the values JASP.MinOnly, JASP.MaxOnly or
  JASP.None.

## Inherited Properties from TextField

- **name** (string) - R option name this control binds to. Default: "".
- **value** (string) - Current text value. Default: "".
- **defaultValue** (var) - Default value. Default: 0.
- **label** (string) - Label displayed before the field. Default: "".
- **afterLabel** (string) - Label displayed after the field. Default:
  "".
- **fieldWidth** (int) - Width of the input field. Default:
  jaspTheme.numericFieldWidth.

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
IntegerField {
    name: "sampleSize"
    label: qsTr("Sample size")
    defaultValue: 100
    min: 1
}
```
