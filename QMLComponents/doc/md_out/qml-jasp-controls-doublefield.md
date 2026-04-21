[JASP.Controls](jasp-controls-qmlmodule.md)

DoubleField


# DoubleField QML Type

A text field preset for entering numeric (double) values.


|  |  |
|----|----|
| Import Statement: | `import JASP.Controls 1.0` |
| Inherits: | [TextField](qml-jasp-controls-textfield.md) |
| Inherited By: | [PercentField](qml-jasp-controls-percentfield.md) |


- [List of all members, including inherited
  members](qml-jasp-controls-doublefield-members.md)

## Detailed Description

Extends
[TextField](qml-jasp-controls-textfield.md)
with the "number" input type and a built-in JASPDoubleValidator.
Supports configurable minimum, maximum, decimal precision, and sign
constraints.

## R Binding

- **R Type:** `numeric`
- **Default:** 0

## Properties

- **negativeValues** (bool) - Allow negative numbers. Default: false.
- **min** (double) - Minimum allowed value. Default: 0 (or -Infinity
  when negativeValues is true).
- **max** (double) - Maximum allowed value. Default: Infinity.
- **decimals** (int) - Number of decimal places. Default: 3.
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
- **fieldWidth** (int) - Width of the input field. Default: 40.

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
DoubleField {
    name: "alpha"
    label: qsTr("Significance level")
    defaultValue: 0.05
    min: 0
    max: 1
    decimals: 3
}
```
