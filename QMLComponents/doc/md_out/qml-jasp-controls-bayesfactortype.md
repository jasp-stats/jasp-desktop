[JASP.Controls](jasp-controls-qmlmodule.md)

BayesFactorType


# BayesFactorType QML Type

A pre-built radio button group for selecting Bayes Factor reporting
format. [More...](#details)


|                   |                                                   |
|-------------------|---------------------------------------------------|
| Import Statement: | `import JASP.Controls 1.0`                        |
| Inherits:         | [RadioButtonGroup](qml-jasp-controls-radiobuttongroup.md)                |


- [List of all members, including inherited
  members](qml-jasp-controls-bayesfactortype-members.md)

## Detailed Description

Provides three options: BF₁₀ (or BF₊₀/BF₋₀ for one-sided), BF₀₁, and
Log(BF₁₀). The subscript signs automatically adjust based on the
correlated property.

## R Binding

- **R Type:** `character`
- **Default:** "BF10"

## Properties

- **name** (string) - R option name this control binds to. Default:
  "bayesFactorType".
- **title** (string) - Title displayed above the group. Default: "Bayes
  Factor".
- **correlated** (string) - Hypothesis direction: "twoSided", "greater",
  or "less". Controls subscript display. Default: "twoSided".

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
BayesFactorType {}
```
