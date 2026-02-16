<a href="jasp-controls-qmlmodule.md" translate="no">JASP.Controls</a>

SimpleTableView

<div class="sidebar">

<div class="toc">

### Contents

- [Detailed Description](#details)
- [R Binding](#r-binding)
- [Inherited Properties from
  BasicThreeButtonTableView](#inherited-properties-from-basicthreebuttontableview)
- [Other Inherited Properties](#other-inherited-properties)
- [Example](#example)

</div>

<div id="sidebar-content" class="sidebar-content">

</div>

</div>

# SimpleTableView QML Type

A table view preset with Add Column, Delete Column, and Reset buttons.
[More...](#details)

<div class="table">

|  |  |
|----|----|
| Import Statement: | `import JASP.Controls 1.0` |
| Inherits: | <a href="qml-jasp-controls-basicthreebuttontableview.md"
translate="no">BasicThreeButtonTableView</a> |

</div>

- [List of all members, including inherited
  members](qml-jasp-controls-simpletableview-members.md)

## Detailed Description

Extends <a href="qml-jasp-controls-basicthreebuttontableview.md"
translate="no">BasicThreeButtonTableView</a> with the Simple model type.
Columns can be added and removed dynamically. Reset restores the initial
column layout.

## R Binding

- **R Type:** data.frame
- **Default:** Table with initialColumnCount columns and initialRowCount
  rows

## Inherited Properties from BasicThreeButtonTableView

- **name** (string) - R option name. Default: "".
- **source** (var) - Source for populating the table.
- **initialColumnCount** (int) - Starting column count. Default: 1.
- **initialRowCount** (int) - Starting row count. Default: 0.

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
SimpleTableView {
    name: "priorMatrix"
    initialColumnCount: 3
    initialRowCount: 3
}
```
