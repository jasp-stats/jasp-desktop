[JASP.Controls](jasp-controls-qmlmodule.md)

IntegerField


# IntegerField QML Type

A text field preset for entering integer values. [More...](#details)


|  |  |
|----|----|
| Import Statement: | `import JASP.Controls 1.0` |
| Inherits: | [TextField](qml-jasp-controls-textfield.md) |


- [List of all members, including inherited
  members](qml-jasp-controls-integerfield-members.md)

## Detailed Description

Extends
[TextField](qml-jasp-controls-textfield.md)
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
