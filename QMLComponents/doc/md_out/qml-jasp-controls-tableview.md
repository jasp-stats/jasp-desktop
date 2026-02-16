<a href="jasp-controls-qmlmodule.md" translate="no">JASP.Controls</a>

TableView

<div class="sidebar">

<div class="toc">

### Contents

- [Detailed Description](#details)
- [R Binding](#r-binding)
- [Properties](#properties)
- [Inherited Properties](#inherited-properties)
- [Example](#example)

</div>

<div id="sidebar-content" class="sidebar-content">

</div>

</div>

# TableView QML Type

A scrollable, editable data table for entering structured values.
[More...](#details)

<div class="table">

|                   |                            |
|-------------------|----------------------------|
| Import Statement: | `import JASP.Controls 1.0` |

</div>

- [List of all members, including inherited
  members](qml-jasp-controls-tableview-members.md)

## Detailed Description

Renders a grid of editable cells with column and row headers,
scrollbars, and optional add/remove column buttons (for GridInput model
type). Each cell uses a <a href="qml-jasp-controls-formulafield.md"
translate="no">FormulaField</a> with configurable validators (integer,
double, string).

## R Binding

- **R Type:** data.frame or matrix
- **Default:** Depends on model type

## Properties

- **name** (string) - R option name this control binds to. Default: "".
- **modelType** (enum) - Table model type (JASP.Simple, JASP.GridInput,
  JASP.CustomContrasts, etc.). Default: depends on usage.
- **defaultValue** (string) - Default value for new cells. Default:
  depends on modelType.
- **initialColumnCount** (int) - Starting column count. Default: depends
  on modelType.
- **cornerText** (string) - Text in the top-left corner cell. Default:
  "Row \#".
- **minimum** (double) - Minimum numeric value for cells. Default: 0.
- **decimals** (int) - Decimal places for double validation. Default: 1.
- **isFirstColEditable** (bool) - Whether the first column is editable.
  Default: true.
- **showAddRemoveButtons** (bool) - Show column add/remove buttons.
  Default: true for GridInput.
- **factorsSource** (string) - Source for factor columns. Default: "".
- **filter** (string) - R expression filtering rows. Default: "rep(TRUE,
  rowcount)".

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
TableView {
    name: "coefficients"
    modelType: JASP.GridInput
    initialColumnCount: 3
}
```
