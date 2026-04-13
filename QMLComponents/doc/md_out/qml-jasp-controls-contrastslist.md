[JASP.Controls](jasp-controls-qmlmodule.md)

ContrastsList


# ContrastsList QML Type

A composite control for specifying contrasts for factor variables.


|                   |                            |
|-------------------|----------------------------|
| Import Statement: | `import JASP.Controls 1.0` |


- [List of all members, including inherited
  members](qml-jasp-controls-contrastslist-members.md)

## Detailed Description

Combines a [VariablesList](qml-jasp-controls-variableslist.md) (showing factors with contrast type
dropdowns) and an optional
[CustomContrastsTableView](qml-jasp-controls-customcontraststableview.md) for entering custom contrast
weights. Each factor gets a dropdown to select a contrast type (none,
deviation, simple, difference, Helmert, repeated, polynomial, or
custom).

## R Binding

- **R Type:** list (contrasts per factor, plus custom contrast matrices)
- **Default:** All factors set to "none"

## Properties

- **factorsSourceName** (string) - Name of the source control providing
  factor variables. Default: "fixedFactors".
- **source** (var) - Alias for the internal
  [VariablesList](qml-jasp-controls-variableslist.md) source.
- **repeatedMeasureFactors** (string) - Name of the repeated measures
  factors source. Default: "repeatedMeasuresFactors".
- **addCustom** (bool) - Whether to include "custom" as a contrast
  option. Default: true.
- **contrastValues** (array) - Available contrast types. Default: none,
  deviation, simple, difference, Helmert, repeated, polynomial, custom.

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
ContrastsList {
    factorsSourceName: "fixedFactors"
}
```
