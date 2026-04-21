[JASP.Controls](jasp-controls-qmlmodule.md)

PercentField


# PercentField QML Type

A numeric field preset for entering percentage values (0–100).


|  |  |
|----|----|
| Import Statement: | `import JASP.Controls 1.0` |
| Inherits: | [DoubleField](qml-jasp-controls-doublefield.md) |
| Inherited By: | [CIField](qml-jasp-controls-cifield.md) |


- [List of all members, including inherited
  members](qml-jasp-controls-percentfield-members.md)

## Detailed Description

Extends [DoubleField](qml-jasp-controls-doublefield.md) with the "percent" input type, 0 decimal
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
