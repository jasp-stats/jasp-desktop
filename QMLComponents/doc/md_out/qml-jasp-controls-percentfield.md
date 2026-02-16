<a href="jasp-controls-qmlmodule.md" translate="no">JASP.Controls</a>

PercentField

<div class="sidebar">

<div class="toc">

### Contents

- [Detailed Description](#details)
- [R Binding](#r-binding)
- [Properties](#properties)
- [Inherited Properties from
  DoubleField](#inherited-properties-from-doublefield)
- [Other Inherited Properties](#other-inherited-properties)
- [Example](#example)

</div>

<div id="sidebar-content" class="sidebar-content">

</div>

</div>

# PercentField QML Type

A numeric field preset for entering percentage values (0–100).
[More...](#details)

<div class="table">

|  |  |
|----|----|
| Import Statement: | `import JASP.Controls 1.0` |
| Inherits: | <a href="qml-jasp-controls-doublefield.md"
translate="no">DoubleField</a> |
| Inherited By: | <a href="qml-jasp-controls-cifield.md" translate="no">CIField</a> |

</div>

- [List of all members, including inherited
  members](qml-jasp-controls-percentfield-members.md)

## Detailed Description

Extends <a href="qml-jasp-controls-doublefield.md"
translate="no">DoubleField</a> with the "percent" input type, 0 decimal
places, and a "%" suffix label. Min is 0, max is 100.

## R Binding

- **R Type:** `numeric` (percentage)
- **Default:** 50

## Properties

- **showPercent** (bool) - Show the "%" suffix after the field. Default:
  true.

## Inherited Properties from DoubleField

- **name** (string) - R option name this control binds to. Default: "".
- **defaultValue** (var) - Default value. Default: 50.
- **min** (double) - Minimum value. Default: 0.
- **max** (double) - Maximum value. Default: 100.
- **decimals** (int) - Decimal places. Default: 0.
- **label** (string) - Label displayed before the field. Default: "".

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
PercentField {
    name: "confidenceLevel"
    label: qsTr("Confidence interval")
    defaultValue: 95
}
```
