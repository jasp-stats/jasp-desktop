[JASP.Controls](jasp-controls-qmlmodule.md)

FactorsForm


# FactorsForm QML Type

A form for defining latent factors by assigning observed variables.


|                   |                            |
|-------------------|----------------------------|
| Import Statement: | `import JASP.Controls 1.0` |


- [List of all members, including inherited
  members](qml-jasp-controls-factorsform-members.md)

## Detailed Description

Combines an [AvailableVariablesList](qml-jasp-controls-availablevariableslist.md) with dynamically created
[FactorsList](qml-jasp-controls-factorslist.md) panels. Users can add or remove factors
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
