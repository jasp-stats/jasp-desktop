[JASP.Controls](jasp-controls-qmlmodule.md)

ComputedColumnField


# ComputedColumnField QML Type

A text field for entering a computed column name. [More...](#details)


|  |  |
|----|----|
| Import Statement: | `import JASP.Controls 1.0` |
| Inherits: | [TextField](qml-jasp-controls-textfield.md) |


- [List of all members, including inherited
  members](qml-jasp-controls-computedcolumnfield-members.md)

## Detailed Description

Extends
[TextField](qml-jasp-controls-textfield.md)
with the "computedColumn" input type. The entered name is used to create
or reference a computed column in the dataset. Validation ensures the
column name is valid and available.

## R Binding

- **R Type:** `character`
- **Default:** ""

## Inherited Properties from TextField

- **name** (string) - R option name this control binds to. Default: "".
- **value** (string) - Current text value. Default: "".
- **label** (string) - Label displayed before the field. Default: "".
- **afterLabel** (string) - Label displayed after the field. Default:
  "".
- **fieldWidth** (int) - Width of the input field. Default: 200.
- **placeholderText** (string) - Greyed text shown when field is empty.
  Default: "".

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
ComputedColumnField {
    name: "generatedColumn"
    label: qsTr("Column name")
}
```
