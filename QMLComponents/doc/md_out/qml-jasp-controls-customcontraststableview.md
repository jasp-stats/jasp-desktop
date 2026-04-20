[JASP.Controls](jasp-controls-qmlmodule.md)

CustomContrastsTableView


# CustomContrastsTableView QML Type

A table view preset for entering custom contrast weight matrices.


|  |  |
|----|----|
| Import Statement: | `import JASP.Controls 1.0` |
| Inherits: | [BasicThreeButtonTableView](qml-jasp-controls-basicthreebuttontableview.md) |


- [List of all members, including inherited
  members](qml-jasp-controls-customcontraststableview-members.md)

## Detailed Description

Extends [BasicThreeButtonTableView](qml-jasp-controls-basicthreebuttontableview.md) with the CustomContrasts
model type. Buttons are configured as Add Contrast, Delete Contrast, and
Reset, placed in a row above the table. Rows are automatically populated
from the factor levels.

## R Binding

- **R Type:** matrix
- **Default:** Identity contrast matrix matching factor levels

## Inherited Properties from BasicThreeButtonTableView

- **name** (string) - R option name this control binds to. Default: "".
- **source** (var) - Source for populating the table.
- **factorsSource** (var) - Source providing factor level information
  for row headers.
- **itemType** (var) - Data type for cell values. Default: JASP.Double.
- **minimum** (var) - Minimum allowed value for cells. Default:
  -Infinity.
- **decimals** (int) - Number of decimal places. Default: 3.
- **columnName** (string) - Column name alias for the contrast variable.
- **buttonsInRow** (bool) - Place buttons in a row above the table.
  Default: true.

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
CustomContrastsTableView {
    name: "values"
    columnName: "myFactor"
    factorsSource: "repeatedMeasuresFactors"
}
```
