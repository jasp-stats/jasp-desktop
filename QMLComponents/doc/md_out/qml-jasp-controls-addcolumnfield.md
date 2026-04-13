[JASP.Controls](jasp-controls-qmlmodule.md)

AddColumnField


# AddColumnField QML Type

A text input field that creates a new computed column in the dataset.


|  |  |
|----|----|
| Import Statement: | `import JASP.Controls 1.0` |
| Inherits: | [TextField](qml-jasp-controls-textfield.md) |


- [List of all members, including inherited
  members](qml-jasp-controls-addcolumnfield-members.md)

## Detailed Description

Extends
[TextField](qml-jasp-controls-textfield.md)
with inputType set to "addColumn". The entered name becomes the name of
a new column added to the dataset by the analysis.

- **R Type:** `character`
- **Default:** ""

## Properties

- **name** (string) - R option name this control binds to. Default: "".
- **columnType** (int) - The type of the new column: columnTypeScale,
  columnTypeNominal, or columnTypeOrdinal. Default: columnTypeScale.

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
AddColumnField {
    name: "residuals"
    columnType: columnTypeScale
}
```
