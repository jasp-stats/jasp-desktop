<a href="jasp-controls-qmlmodule.md" translate="no">JASP.Controls</a>

JagsTableView

<div class="sidebar">

<div class="toc">

### Contents

- [Detailed Description](#details)
- [R Binding](#r-binding)
- [Properties](#properties)
- [Inherited Properties from
  BasicThreeButtonTableView](#inherited-properties-from-basicthreebuttontableview)
- [Other Inherited Properties](#other-inherited-properties)
- [Example](#example)

</div>

<div id="sidebar-content" class="sidebar-content">

</div>

</div>

# JagsTableView QML Type

A table view preset for entering JAGS data. [More...](#details)

<div class="table">

|  |  |
|----|----|
| Import Statement: | `import JASP.Controls 1.0` |
| Inherits: | <a href="qml-jasp-controls-basicthreebuttontableview.md"
translate="no">BasicThreeButtonTableView</a> |

</div>

- [List of all members, including inherited
  members](qml-jasp-controls-jagstableview-members.md)

## Detailed Description

Extends <a href="qml-jasp-controls-basicthreebuttontableview.md"
translate="no">BasicThreeButtonTableView</a> with the JAGSDataInput
model type. Buttons are configured as Add Data, Delete Data, and Reset.
Cell values are strings. Row count is capped by maxDataEntries.

## R Binding

- **R Type:** data.frame
- **Default:** Empty table with 2 columns

## Properties

- **maxDataEntries** (int) - Maximum number of data rows allowed.
  Default: 30.

## Inherited Properties from BasicThreeButtonTableView

- **name** (string) - R option name this control binds to. Default: "".
- **source** (var) - Source for populating the table.
- **initialColumnCount** (int) - Number of columns at creation. Default:
  2.
- **initialRowCount** (int) - Number of rows at creation. Default: 0.

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
JagsTableView {
    name: "dataInput"
    maxDataEntries: 50
}
```
