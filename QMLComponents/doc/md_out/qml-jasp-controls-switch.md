[JASP.Controls](jasp-controls-qmlmodule.md)

Switch


# Switch QML Type

A toggle switch control. [More...](#details)


|                   |                            |
|-------------------|----------------------------|
| Import Statement: | `import JASP.Controls 1.0` |


- [List of all members, including inherited
  members](qml-jasp-controls-switch-members.md)

## Detailed Description

A sliding toggle indicator that can be used instead of a checkbox. Binds
a boolean value to R, similar to
[CheckBox](qml-jasp-controls-checkbox.md)
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
