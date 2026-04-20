[JASP.Controls](jasp-controls-qmlmodule.md)

FactorLevelList


# FactorLevelList QML Type

A grid editor for defining factors and their levels. [More...](#details)


|                   |                            |
|-------------------|----------------------------|
| Import Statement: | `import JASP.Controls 1.0` |


- [List of all members, including inherited
  members](qml-jasp-controls-factorlevellist-members.md)

## Detailed Description

Displays a scrollable grid where users can name factors and their
levels. New factors and levels are added by typing in placeholder rows.
Existing items can be renamed or deleted. Used in repeated-measures
ANOVA and similar analyses.

## R Binding

- **R Type:** list (named lists of factor levels)
- **Default:** One factor with two levels

## Properties

- **name** (string) - R option name this control binds to. Default: "".
- **title** (string) - Title displayed above the list. Default: "".
- **factorName** (string) - Base label for factor rows. Default:
  "Factor".
- **levelName** (string) - Base label for level rows. Default: "Level".
- **factorPlaceHolder** (string) - Placeholder text for new factor rows.
  Default: "New Factor".
- **levelPlaceHolder** (string) - Placeholder text for new level rows.
  Default: "New Level".
- **minFactors** (int) - Minimum number of factors. Default: 1.
- **minLevels** (int) - Minimum number of levels per factor. Default: 2.

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
FactorLevelList {
    name: "repeatedMeasuresFactors"
}
```
