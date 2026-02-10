<a href="jasp-controls-qmlmodule.md" translate="no">JASP.Controls</a>

TextField

<div class="sidebar">

<div class="toc">

### Contents

- [Detailed Description](#details)
- [Properties](#properties)
- [Inherited Properties](#inherited-properties)
- [Signals](#signals)
- [Example](#example)

</div>

<div id="sidebar-content" class="sidebar-content">

</div>

</div>

# TextField QML Type

A single-line text input control for entering strings.
[More...](#details)

<div class="table">

|  |  |
|----|----|
| Import Statement: | `import JASP.Controls 1.0` |
| Inherited By: | <a href="qml-jasp-controls-addcolumnfield.md"
translate="no">AddColumnField</a>, <a href="qml-jasp-controls-checkcolumnisfreeorminefield.md"
translate="no">CheckColumnIsFreeOrMineField</a>, <a href="qml-jasp-controls-computedcolumnfield.md"
translate="no">ComputedColumnField</a>, <a href="qml-jasp-controls-doublefield.md"
translate="no">DoubleField</a>, <a href="qml-jasp-controls-fileselector.md"
translate="no">FileSelector</a>, <a href="qml-jasp-controls-formulafield.md"
translate="no">FormulaField</a>, and <a href="qml-jasp-controls-integerfield.md"
translate="no">IntegerField</a> |

</div>

- [List of all members, including inherited
  members](qml-jasp-controls-textfield-members.md)

## Detailed Description

For numeric input, use <a href="qml-jasp-controls-integerfield.md"
translate="no">IntegerField</a>,
<a href="qml-jasp-controls-doublefield.md"
translate="no">DoubleField</a>, or
<a href="qml-jasp-controls-percentfield.md"
translate="no">PercentField</a>.

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
