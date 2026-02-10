<a href="jasp-controls-qmlmodule.md" translate="no">JASP.Controls</a>

FactorsForm

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

# FactorsForm QML Type

A form for defining latent factors by assigning observed variables.
[More...](#details)

<div class="table">

|                   |                            |
|-------------------|----------------------------|
| Import Statement: | `import JASP.Controls 1.0` |

</div>

- [List of all members, including inherited
  members](qml-jasp-controls-factorsform-members.md)

## Detailed Description

Combines an <a href="qml-jasp-controls-availablevariableslist.md"
translate="no">AvailableVariablesList</a> with dynamically created
<a href="qml-jasp-controls-factorslist.md"
translate="no">FactorsList</a> panels. Users can add or remove factors
and assign variables to each using assign buttons. Commonly used in
Exploratory/Confirmatory Factor Analysis.

## R Binding

- **R Type:** list (one entry per factor containing name, title, and
  assigned variable names)
- **Default:** One empty factor

## Properties

- **name** (string) - R option name this control binds to. Default: "".
- **availableVariablesListName** (string) - Name of the available
  variables source. Default: "allAvailableVariables".
- **allowedColumns** (array) - Column types allowed for assignment.
  Default: \["scale"\].
- **initNumberFactors** (int) - Initial number of factor lists shown.
  Default: 1.
- **baseName** (string) - Base name for factor R option keys (not
  translated). Default: "Factor".
- **baseTitle** (string) - Base display title for factors (translated).
  Default: "Factor".
- **startIndex** (int) - Starting index for factor numbering. Default:
  1.
- **allowInteraction** (bool) - Allow interaction terms in factor lists.
  Default: false.
- **allowTypeChange** (bool) - Allow changing variable type icons.
  Default: false.
- **addInteractionsByDefault** (bool) - Automatically add interactions.
  Default: false.
- **nested** (bool) - Use nested factor structure. Default: false.

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
FactorsForm {
    name: "factors"
    initNumberFactors: 2
    allowedColumns: ["scale"]
}
```
