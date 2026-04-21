[JASP.Controls](jasp-controls-qmlmodule.md)

Slider


# Slider QML Type

A slider with a linked numeric text field. [More...](#details)


|                   |                            |
|-------------------|----------------------------|
| Import Statement: | `import JASP.Controls 1.0` |


- [List of all members, including inherited
  members](qml-jasp-controls-slider-members.md)

## Detailed Description

Backed by SliderBase. Displays a draggable slider alongside a
[DoubleField](qml-jasp-controls-doublefield.md) that stays synchronized. Supports
vertical and horizontal orientations.

## R Binding

- **R Type:** `numeric`
- **Default:** 0.5

## Properties

- **name** (string) - R option name this control binds to. Default: "".
- **label** (string) - Label displayed above the slider. Default: "".
- **value** (real) - Current slider value. Default: 0.5.
- **min** (real) - Minimum value. Alias: from. Default: 0.
- **max** (real) - Maximum value. Alias: to. Default: 1.
- **stepSize** (real) - Step increment. Default: 1 / 10^decimals.
- **decimals** (int) - Decimal places for the text field. Default: 2.
- **vertical** (bool) - Use vertical orientation. Default: true.

## Inherited Properties

- **enabled** (bool) - Whether the control is interactive. Default:
  true.
- **visible** (bool) - Whether the control is visible. Default: true.
- **info** (string) - Info that will be used by tooltip and to generate
  the help. Default: "".
- **toolTip** (string) - This property overwrite info property, in order
  to display a simpler tooltip text. Default: "".

## Example

``` qml
Slider {
    name: "prior"
    label: qsTr("Prior width")
    value: 0.707
    min: 0
    max: 2
}
```
