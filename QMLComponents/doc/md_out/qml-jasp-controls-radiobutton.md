[JASP.Controls](jasp-controls-qmlmodule.md)

RadioButton


# RadioButton QML Type

A radio button option within a
[RadioButtonGroup](qml-jasp-controls-radiobuttongroup.md). [More...](#details)


|                   |                            |
|-------------------|----------------------------|
| Import Statement: | `import JASP.Controls 1.0` |


- [List of all members, including inherited
  members](qml-jasp-controls-radiobutton-members.md)

## Detailed Description

Backed by RadioButtonBase. When checked, its name is sent as the value
of the parent [RadioButtonGroup](qml-jasp-controls-radiobuttongroup.md). Supports nested child controls that
can be enabled only when this radio button is selected.

## R Binding

- **R Type:** Used as a value option within a
  [RadioButtonGroup](qml-jasp-controls-radiobuttongroup.md) (string).
- **Default:** unchecked

## Properties

- **name** (string) - Value sent to R when this option is selected.
  Alias: value. Default: "".
- **label** (string) - Text displayed next to the radio indicator.
  Alias: text. Default: "".
- **checked** (bool) - Whether this radio button is currently selected.
  Default: false.
- **childrenOnSameRow** (bool) - Place child controls on the same row.
  Default: false.
- **enableChildrenOnChecked** (bool) - Only enable child controls when
  checked. Default: true.
- **indentChildren** (bool) - Indent child controls below the label.
  Default: true.
- **columns** (int) - Number of columns in the child controls area.
  Default: 1.

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
RadioButtonGroup {
    name: "hypothesis"
    RadioButton { value: "twoSided"; label: qsTr("≠ Test value"); checked: true }
    RadioButton { value: "greater";  label: qsTr("> Test value") }
    RadioButton { value: "less";     label: qsTr("< Test value") }
}
```

``` qml
RadioButtonGroup {
        title: qsTr("Operation")
        name: "operation"
        RadioButton {
                value: "plus"; label: qsTr("Plus"); checked: true
                DoubleField { label: "Extra Quantity"; name: "plusExtraQuantity"} // This DoubleField will be eanbled only if Plus option is checked
        }
        RadioButton { value: "Multiply";  label: qsTr("Multiply") }
        RadioButton { value: "Divide";    label: qsTr("Divide") }
}
```
