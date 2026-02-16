<a href="jasp-controls-qmlmodule.md" translate="no">JASP.Controls</a>

FormulaField

<div class="sidebar">

<div class="toc">

### Contents

- [Detailed Description](#details)
- [R Binding](#r-binding)
- [Properties](#properties)
- [Inherited Properties from
  TextField](#inherited-properties-from-textfield)
- [Other Inherited Properties](#other-inherited-properties)
- [Example](#example)

</div>

<div id="sidebar-content" class="sidebar-content">

</div>

</div>

# FormulaField QML Type

A text field preset for entering R-style formulas. [More...](#details)

<div class="table">

|  |  |
|----|----|
| Import Statement: | `import JASP.Controls 1.0` |
| Inherits: | <a href="qml-jasp-controls-textfield.md" translate="no">TextField</a> |

</div>

- [List of all members, including inherited
  members](qml-jasp-controls-formulafield-members.md)

## Detailed Description

Extends
<a href="qml-jasp-controls-textfield.md" translate="no">TextField</a>
with the "formula" input type. Formulas are evaluated by the R engine
and validated against configurable min/max bounds. The computed numeric
result is accessible via realValue.

## R Binding

- **R Type:** `character` (formula string, evaluated to numeric)
- **Default:** "0"

## Properties

- **realValue** (double) - The evaluated numeric result of the formula.
  Read-only.
- **realValues** (array) - Array of evaluated results when formula
  yields multiple values. Read-only.
- **min** (double) - Minimum allowed evaluated value. Default:
  -Infinity.
- **max** (double) - Maximum allowed evaluated value. Default: Infinity.
- **inclusive** (enum) - Whether min/max bounds are inclusive. Default:
  JASP.MinMax. Can have also the values JASP.MinOnly, JASP.MaxOnly or
  JASP.None.
- **parseDefaultValue** (bool) - Whether the default value should be
  parsed as a formula. Default: true.

## Inherited Properties from TextField

- **name** (string) - R option name this control binds to. Default: "".
- **value** (string) - Current formula text. Default: "".
- **defaultValue** (var) - Default formula text. Default: "0".
- **label** (string) - Label displayed before the field. Default: "".
- **afterLabel** (string) - Label displayed after the field. Default:
  "".
- **fieldWidth** (int) - Width of the input field. Default:
  jaspTheme.textFieldWidth / 2.

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
FormulaField { // The user can here type '1/3' or 'sin(10)'
    name: "priorMean"
    label: qsTr("Prior mean")
    defaultValue: "0"
}
```
