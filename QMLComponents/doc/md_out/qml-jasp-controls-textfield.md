[JASP.Controls](jasp-controls-qmlmodule.md)

TextField


# TextField QML Type

A single-line text input control for entering strings.


|  |  |
|----|----|
| Import Statement: | `import JASP.Controls 1.0` |
| Inherited By: | [AddColumnField](qml-jasp-controls-addcolumnfield.md), [CheckColumnIsFreeOrMineField](qml-jasp-controls-checkcolumnisfreeorminefield.md), [ComputedColumnField](qml-jasp-controls-computedcolumnfield.md), [DoubleField](qml-jasp-controls-doublefield.md), [FileSelector](qml-jasp-controls-fileselector.md), [FormulaField](qml-jasp-controls-formulafield.md), and [IntegerField](qml-jasp-controls-integerfield.md) |


- [List of all members, including inherited
  members](qml-jasp-controls-textfield-members.md)

## Detailed Description

For numeric input, use [IntegerField](qml-jasp-controls-integerfield.md),
[DoubleField](qml-jasp-controls-doublefield.md), or
[PercentField](qml-jasp-controls-percentfield.md).

- **R Type:** `character`
- **Default:** "" or value of defaultValue property

## Properties

- **name** (string) - R option name this control binds to. Default: "".
- **value** (string) - Current text value. Default: "".
- **label** (string) - Label displayed before the field. Default: "".
- **afterLabel** (string) - Label displayed after the field. Default:
  "".
- **defaultValue** (var) - Value restored when field is empty on blur.
- **placeholderText** (string) - Greyed text shown when field is empty.
  Default: "".
- **fieldWidth** (int) - Width of the input field. Default: 200.
- **selectValueOnFocus** (bool) - Select all text when focused. Default:
  false.
- **editable** (bool) - Whether user can edit the text. Default: true.

## Inherited Properties

- **enabled** (bool) - Whether the control is interactive. Default:
  true.
- **visible** (bool) - Whether the control is visible. Default: true.
- **info** (string) - Info that will be used by tooltip and to generate
  the help. Default: "".
- **toolTip** (string) - This property overwrite info property, in order
  to display a simpler tooltip text. Default: "".

## Signals

- **editingFinished()** - Emitted when user completes editing (blur or
  Enter).
- **textEdited()** - Emitted on each keystroke.

## Example

``` qml
TextField {
    name: "tableTitle"
    label: qsTr("Table title")
    placeholderText: qsTr("Enter title...")
    fieldWidth: 200
}
```
