[JASP.Controls](jasp-controls-qmlmodule.md)

CheckBox


# CheckBox QML Type

A boolean toggle control that binds a true/false value to an R option.


|                   |                            |
|-------------------|----------------------------|
| Import Statement: | `import JASP.Controls 1.0` |


- [List of all members, including inherited
  members](qml-jasp-controls-checkbox-members.md)

## Detailed Description

CheckBox can optionally contain child controls that become enabled when
checked.

## R Binding

- **R Type:** `logical`
- **Default:** `FALSE`

## Properties

- **name** (string) - R option name this control binds to. Default: "".
- **checked** (bool) - Current checked state. Default: false.
- **label** (string) - Text displayed next to the checkbox. Default: "".
- **childrenOnSameRow** (bool) - If true, child controls layout
  horizontally. Default: false.
- **enableChildrenOnChecked** (bool) - If true, children enabled only
  when checked. Default: true.
- **columns** (int) - Number of columns for child controls layout.
  Default: 1.

## Inherited Properties

- **enabled** (bool) - Whether the control is interactive. Default:
  true.
- **visible** (bool) - Whether the control is visible. Default: true.
- **info** (string) - Info that will be used by tooltip and to generate
  the help. Default: "".
- **toolTip** (string) - This property overwrite info property, in order
  to display a simpler tooltip text. Default: "".

## Signals

- **clicked()** - Emitted when user clicks the checkbox.

## Example

``` qml
CheckBox {
    name: "includeCI"
    label: qsTr("Confidence interval")
    checked: true

    CIField {
        name: "ciWidth"
    }
}
```
