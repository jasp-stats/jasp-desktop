<a href="jasp-controls-qmlmodule.md" translate="no">JASP.Controls</a>

CIField

<div class="sidebar">

<div class="toc">

### Contents

- [Detailed Description](#details)
- [R Binding](#r-binding)
- [Properties](#properties)
- [Inherited Properties](#inherited-properties)
- [Example](#example)

</div>

<div id="sidebar-content" class="sidebar-content">

</div>

</div>

# CIField QML Type

A percentage input field preset for confidence interval width.
[More...](#details)

<div class="table">

|                   |                                               |
|-------------------|-----------------------------------------------|
| Import Statement: | `import JASP.Controls 1.0`                    |
| Inherits:         | <a href="qml-jasp-controls-percentfield.md" 
                     translate="no">PercentField</a>                |

</div>

- [List of all members, including inherited
  members](qml-jasp-controls-cifield-members.md)

## Detailed Description

Extends <a href="qml-jasp-controls-percentfield.md"
translate="no">PercentField</a> with defaults suitable for confidence
interval input: 95% default value, 1 decimal place, and exclusive bounds
(0-100 not included).

## R Binding

- **R Type:** `numeric`
- **Default:** 95

## Properties

- **name** (string) - R option name this control binds to. Default: "".
- **label** (string) - Label displayed before the field. Default: "".
- **defaultValue** (var) - Default percentage value. Default: 95.
- **decimals** (int) - Number of decimal places. Default: 1.

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
CheckBox {
    name: "includeCI"
    label: qsTr("Confidence interval")

    CIField {
        name: "ciWidth"
    }
}
```
