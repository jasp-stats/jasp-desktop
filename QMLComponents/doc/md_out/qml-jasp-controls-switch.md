<a href="jasp-controls-qmlmodule.md" translate="no">JASP.Controls</a>

Switch

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

# Switch QML Type

A toggle switch control. [More...](#details)

<div class="table">

|                   |                            |
|-------------------|----------------------------|
| Import Statement: | `import JASP.Controls 1.0` |

</div>

- [List of all members, including inherited
  members](qml-jasp-controls-switch-members.md)

## Detailed Description

A sliding toggle indicator that can be used instead of a checkbox. Binds
a boolean value to R, similar to
<a href="qml-jasp-controls-checkbox.md" translate="no">CheckBox</a>
but with a different visual appearance.

## R Binding

- **R Type:** `logical`
- **Default:** false

## Properties

- **name** (string) - R option name this control binds to. Default: "".
- **label** (string) - Text displayed next to the switch. Default: "".
- **checked** (bool) - Whether the switch is on. Default: false.

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
Switch {
    name: "darkMode"
    label: qsTr("Dark mode")
}
```
