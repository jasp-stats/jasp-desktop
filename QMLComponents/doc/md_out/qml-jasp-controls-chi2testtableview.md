<a href="jasp-controls-qmlmodule.md" translate="no">JASP.Controls</a>

Chi2TestTableView

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

# Chi2TestTableView QML Type

A table view preset for entering multinomial chi-squared hypotheses.
[More...](#details)

<div class="table">

|  |  |
|----|----|
| Import Statement: | `import JASP.Controls 1.0` |
| Inherits: | <a href="qml-jasp-controls-basicthreebuttontableview.md"
translate="no">BasicThreeButtonTableView</a> |

</div>

- [List of all members, including inherited
  members](qml-jasp-controls-chi2testtableview-members.md)

## Detailed Description

Extends <a href="qml-jasp-controls-basicthreebuttontableview.md"
translate="no">BasicThreeButtonTableView</a> with the MultinomialChi2
model type. The three buttons are configured as Add Column, Delete
Column, and Reset. Column headers default to "H₀ (a)", "H₀ (b)", etc.

## R Binding

- **R Type:** matrix
- **Default:** Empty table

## Properties

- **maxNumHypotheses** (int) - Maximum number of hypothesis columns
  allowed. Default: 10.
- **colHeader** (string) - Custom column header text. When empty, uses
  "H₀ (a)", "H₀ (b)", etc. Default: "".

## Inherited Properties from BasicThreeButtonTableView

- **name** (string) - R option name this control binds to. Default: "".
- **source** (var) - Source for populating the table.
- **initialRowCount** (int) - Number of rows at creation.
- **defaultValue** (var) - Default value for new cells.
- **buttonsInRow** (bool) - Place buttons in a row above the table.
  Default: false.

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
Chi2TestTableView {
    name: "tableWidget"
    maxNumHypotheses: 5
}
```
