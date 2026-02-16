<a href="jasp-controls-qmlmodule.md" translate="no">JASP.Controls</a>

AddColumnField

<div class="sidebar">

<div class="toc">

### Contents

- [Detailed Description](#details)
- [Properties](#properties)
- [Inherited Properties](#inherited-properties)
- [Example](#example)

</div>

<div id="sidebar-content" class="sidebar-content">

</div>

</div>

# AddColumnField QML Type

A text input field that creates a new computed column in the dataset.
[More...](#details)

<div class="table">

|  |  |
|----|----|
| Import Statement: | `import JASP.Controls 1.0` |
| Inherits: | <a href="qml-jasp-controls-textfield.md" translate="no">TextField</a> |

</div>

- [List of all members, including inherited
  members](qml-jasp-controls-addcolumnfield-members.md)

## Detailed Description

Extends
<a href="qml-jasp-controls-textfield.md" translate="no">TextField</a>
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
